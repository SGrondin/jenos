open! Core_kernel
open Cohttp
open Cohttp_lwt_unix
open Config
open Config.Send_curses_config
open Lwt.Syntax
open Result.Monad_infix

let reddit_latch = Latch.create ~cooldown:(Latch.Time.sec 1L)

let delay_latch = Latch.create ~cooldown:(Latch.Time.sec 2L)

let max_reddit_call_time = 6.0

let expire = Latch.Time.min 30L

type curse = {
  title: string;
  url: Discord.Data.Basics.Url.t;
  awards: Reddit.Post.Award.t list;
}
[@@deriving sexp]

type posts = (int64 * curse) String.Map.t [@@deriving sexp]

type state = {
  queue: posts;
  used: String.Set.t;
  regex: Regex.t;
}
[@@deriving sexp]

let make_regex triggers =
  let phrase s = [| sprintf "(?:^%s)" s; sprintf "(?:%s[^a-z]*$)" s |] in
  let word s = [| sprintf "(?:^%s[^a-z]*$)" s |] in
  triggers
  |> List.map ~f:(function
       | Phrase s -> phrase s
       | Word s -> word s)
  |> Array.concat
  |> String.concat_array ~sep:"|"
  |> Regex.create

let initial_state send_curses =
  { queue = String.Map.empty; used = String.Set.empty; regex = make_regex send_curses.triggers }

let fetch ~now subreddit =
  let* () = Latch.wait_and_trigger ~now reddit_latch in
  let uri =
    Uri.make ~scheme:"https" ~host:"www.reddit.com"
      ~path:(sprintf "/r/%s/top.json" subreddit)
      ~query:[ "sort", [ "top" ]; "t", [ "day" ] ]
      ()
  in
  let* () = Lwt_io.printlf !"GET %{Uri}" uri in
  let* res, body = Client.get uri in
  match Response.status res with
  | `OK ->
    let+ data = Cohttp_lwt.Body.to_string body in
    Result.try_with (fun () -> Yojson.Safe.from_string data)
    |> Result.map_error ~f:Exn.to_string
    >>= [%of_yojson: Reddit.Listing.t]
    >>| fun listing ->
    List.fold listing.posts ~init:String.Map.empty ~f:(fun acc -> function
      | { post_hint = Image; is_meta = false; is_video = false; is_self = false; id; title; url; awards }
        ->
        String.Map.set acc ~key:id ~data:(now, { title; url; awards })
      | _ -> acc)
  | status ->
    Lwt_result.fail (sprintf !"Couldn't fetch %{Uri}, received: %{Code.string_of_status}" uri status)

let get_curse ~now { subreddits; phrase_empty; _ } ({ queue; used; _ } as state) =
  let cutoff = Int64.(now - expire) in
  let queue = String.Map.filter queue ~f:Int64.((fun (i64, _curse) -> i64 >= cutoff)) in
  let+ res =
    match String.Map.is_empty queue with
    | true -> (
      let+ fetched =
        Lwt_unix.with_timeout max_reddit_call_time (fun () -> Lwt_list.map_p (fetch ~now) subreddits)
      in
      match List.partition_result fetched with
      | ll, [] ->
        List.fold ll ~init:queue ~f:(fun init curses ->
            String.Map.fold curses ~init ~f:(fun ~key ~data acc ->
                if String.Set.mem used key then acc else String.Map.set acc ~key ~data))
        |> Result.return
      | _, error_s :: _ -> Error error_s
    )
    | false -> Lwt_result.return queue
  in
  res >>= fun queue ->
  match String.Map.min_elt queue with
  | Some (id, (_i64, curse)) ->
    Ok (curse, { state with queue = String.Map.remove queue id; used = String.Set.add used id })
  | None -> Error phrase_empty

let send_curse ~token ~in_background ~channel_id ~now send_curses state =
  let+ data = get_curse ~now send_curses state in
  match data with
  | Error s ->
    in_background (fun () ->
        let* () = Lwt_io.printlf "❌ Curse error: %s" s in
        let* () = Latch.wait_and_trigger delay_latch in
        let+ _msg =
          Rest.Channel.create_message ~token ~channel_id
            ~content:"Something bad happened, please try again in a minute" ()
        in
        ());
    state
  | Ok (curse, state) ->
    let awards =
      let display = function
        | "Gold" -> "🥇"
        | "Silver" -> "🥈"
        | "Bronze" -> "🥉"
        | "Hugz" -> "🤗"
        | s -> s
      in
      match curse.awards with
      | [] -> ""
      | ll ->
        List.map ll ~f:(function
          | { name; count = 1 } -> display name
          | { name; count } -> sprintf {|%s x%d|} (display name) count)
        |> String.concat ~sep:"\n"
        |> sprintf "\n```\n%s\n```"
    in
    let content = sprintf !"%{Uri}%s\n**%s**" curse.url awards curse.title in
    in_background (fun () ->
        let* () = Latch.wait_and_trigger delay_latch in
        let* _msg = Rest.Channel.create_message ~token ~channel_id ~content () in
        let* () = Lwt_unix.sleep 1.0 in
        let+ _msg =
          Rest.Channel.create_message ~token ~channel_id
            ~content:(Array.random_element_exn send_curses.phrases)
            ()
        in
        ());
    state

let on_message_create ~in_background { token; send_curses; _ } state = function
| Data.Message.{ type_ = DEFAULT; channel_id; content; _ }
  when Discord.Data.Basics.Snowflake.equal channel_id send_curses.cursed_channel
       && Regex.matching state.regex content ->
  let now = Latch.Time.get () in
  Latch.trigger ~now delay_latch;
  let* _ind = Rest.Channel.trigger_typing_indicator ~token ~channel_id in
  send_curse ~token ~in_background ~channel_id ~now send_curses state
| _ -> Lwt.return state

let%expect_test "Test curse capture" =
  let regex = make_regex [ Phrase "curse me"; Word "again"; Word "more" ] in
  Regex.matching regex "curse me" |> Bool.to_string |> print_endline;
  [%expect {| true |}];
  Regex.matching regex "just curse me" |> Bool.to_string |> print_endline;
  [%expect {| true |}];
  Regex.matching regex "curse me please" |> Bool.to_string |> print_endline;
  [%expect {| true |}];
  Regex.matching regex "curse me!!!" |> Bool.to_string |> print_endline;
  [%expect {| true |}];
  Regex.matching regex "please curse me!!!" |> Bool.to_string |> print_endline;
  [%expect {| true |}];
  Regex.matching regex "just curse me please" |> Bool.to_string |> print_endline;
  [%expect {| false |}];
  Regex.matching regex "Again!" |> Bool.to_string |> print_endline;
  [%expect {| true |}];
  Regex.matching regex "more" |> Bool.to_string |> print_endline;
  [%expect {| true |}]
