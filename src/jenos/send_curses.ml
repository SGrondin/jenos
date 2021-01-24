open! Core_kernel
open Cohttp
open Cohttp_lwt_unix
open Config
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

type table = (int64 * curse) String.Table.t [@@deriving sexp]

type state = {
  table: table;
  used: String.Set.t;
}
[@@deriving sexp]

let initial_state () = { table = String.Table.create (); used = String.Set.empty }

let fetch ~now subreddit : (table, string) result Lwt.t =
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
    List.filter_map listing.posts ~f:(function
      | { post_hint = Image; is_meta = false; is_video = false; is_self = false; id; title; url; awards }
        ->
        Some (id, (now, { title; url; awards }))
      | _ -> None)
    |> String.Table.of_alist_exn
  | status ->
    Lwt_result.fail (sprintf !"Couldn't fetch %{Uri}, received: %{Code.string_of_status}" uri status)

let get_curse ~now { subreddits; phrase_empty; _ } { table = old_table; used = old_used } =
  let cutoff = Int64.(now - expire) in
  let table = String.Table.filter old_table ~f:Int64.((fun (i64, _curse) -> i64 >= cutoff)) in
  let+ res =
    match String.Table.is_empty table with
    | true -> (
      let+ fetched =
        Lwt_unix.with_timeout max_reddit_call_time (fun () -> Lwt_list.map_p (fetch ~now) subreddits)
      in
      match List.partition_result fetched with
      | ll, [] ->
        List.iter ll ~f:(fun curses ->
            String.Table.iteri curses ~f:(fun ~key ~data ->
                if not (String.Set.mem old_used key) then String.Table.set table ~key ~data))
        |> Result.return
      | _, error_s :: _ -> Error error_s
    )
    | false -> Lwt_result.return ()
  in
  res >>= fun () ->
  match String.Table.choose table with
  | Some (id, (_i64, curse)) ->
    String.Table.remove table id;
    let used = String.Set.add old_used id in
    Ok (curse, { table; used })
  | None -> Error phrase_empty

let regex =
  Regex.create
    {|(?:^curse me)|(?:curse me[^a-z]*$)|(?:^hurt me)|(?:hurt me[^a-z]*$)|(?:^more[^a-z]*$)|(?:^again[^a-z]*$)|}

let send_curse ~in_background ~channel_id ~now ({ token; phrases; _ } as config) state =
  let+ data = get_curse ~now config state in
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
    let content = sprintf !"%{Uri}\n**%s**%s" curse.url curse.title awards in
    in_background (fun () ->
        let* () = Latch.wait_and_trigger delay_latch in
        let* _msg = Rest.Channel.create_message ~token ~channel_id ~content () in
        let* () = Lwt_unix.sleep 1.0 in
        let+ _msg =
          Rest.Channel.create_message ~token ~channel_id ~content:(Array.random_element_exn phrases) ()
        in
        ());
    state

let on_message_create ~in_background ({ token; cursed_channel; _ } as config) state = function
| Data.Message.{ type_ = DEFAULT; channel_id; content; _ }
  when Discord.Data.Basics.Snowflake.equal channel_id cursed_channel && Regex.matching regex content ->
  let now = Latch.Time.get () in
  Latch.trigger ~now delay_latch;
  let* _ind = Rest.Channel.trigger_typing_indicator ~token ~channel_id in
  send_curse ~in_background ~channel_id ~now config state
| _ -> Lwt.return state

let%expect_test "Test curse capture" =
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
  [%expect {| true |}];
  Regex.matching regex "just hurt me" |> Bool.to_string |> print_endline;
  [%expect {| true |}]
