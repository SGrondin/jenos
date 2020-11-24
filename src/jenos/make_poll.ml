open! Core_kernel

open Discord
open Config

let parser =
  let open Angstrom in
  let one =
    let digits = take_while1 (function
      | '0' .. '9' -> true
      | _ -> false
      ) >>| Int.of_string
    in
    let dot = option false (char '.' *> return true) in
    let time = both digits dot >>| function
      | x, dot when x > 12 -> sprintf "%d:%d0pm" (x - 12) (if dot then 3 else 0)
      | x, dot -> sprintf "%d:%d0" x (if dot then 3 else 0)
    in
    (time >>| Option.return) <|> (char '-' *> return None)
  in
  string "Poll! " *> sep_by1 (char ',') one

type letter = A | B | C | D [@@deriving sexp, enum]
let prefix_of_letter = function
| A -> 'A'
| B -> 'B'
| C -> 'C'
| D -> 'D'
let emoji_of_letter = function
| A -> "🇦"
| B -> "🇧"
| C -> "🇨"
| D -> "🇩"
let days = [| "Saturday"; "Saturday"; "Sunday"; "Sunday" |]

let parse raw =
  Angstrom.parse_string parser ~consume:All raw
  |> Result.ok
  |> Option.map ~f:(fun parsed ->
    List.foldi parsed ~init:([], 0) ~f:(fun i ((ll, n) as acc) -> function
    | None -> acc
    | Some x ->
      let letter = letter_of_enum n |> Option.value_exn ~message:(sprintf "Invalid poll option for: %d" n) in
      let s = sprintf "%c: %s %s" (prefix_of_letter letter) (Array.get days i) x in
      (s, letter) :: ll, n + 1
    )
    |> fst
    |> List.rev
    |> List.unzip
    |> Tuple2.map_fst ~f:(String.concat ~sep:"\n")
  )

let%expect_test "Parser" =
  let test s = parse s |> [%sexp_of: (string * letter list) option] |> Sexp.to_string |> print_endline in
  test "Poll! 12,19,-,10";
  [%expect {| (("A: Saturday 12:00\nB: Saturday 7:00pm\nC: Sunday 10:00"(A B C))) |}];
  test "Poll! 7,19.,-,-";
  [%expect {| (("A: Saturday 7:00\nB: Saturday 7:30pm"(A B))) |}];
  test "Poll! -,19,10.";
  [%expect {| (("A: Saturday 7:00pm\nB: Sunday 10:30"(A B))) |}];
  test "Poll 1/2/3/4";
  [%expect {| () |}]

let on_message_create config ~on_exn = function
| Objects.Message.{ id = _; type_ = DEFAULT; channel_id; content; author; _ } when String.(=) author.id config.poll_user_id ->
  parse content |> Option.iter ~f:(fun (text, letters) ->
    Bot.in_background ~on_exn (fun () ->
      let%lwt posted = Rest.Channel.create_message ~token:config.token ~channel_id ~content:text (Parse Objects.Message.of_yojson_exn) in
      Lwt_list.iter_s (fun letter ->
        let%lwt () = Lwt_unix.sleep 2.0 in
        let emoji = emoji_of_letter letter in
        Rest.Channel.create_reaction ~token:config.token ~channel_id ~message_id:posted.id ~emoji Ignore
      ) letters
    )
  )
| _ -> ()
