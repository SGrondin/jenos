open! Core_kernel
open Config

let parser =
  let open Angstrom in
  let digits =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| Int.of_string
  in
  let one =
    let dot = option false (char '.' *> return true) in
    let time =
      both digits dot >>| function
      | 0, dot -> sprintf "0:%d0am" (if dot then 3 else 0)
      | 12, dot -> sprintf "12:%d0pm" (if dot then 3 else 0)
      | x, dot when x > 12 -> sprintf "%d:%d0pm" (x - 12) (if dot then 3 else 0)
      | x, dot -> sprintf "%d:%d0" x (if dot then 3 else 0)
    in
    time >>| Option.return <|> char '-' *> return None
  in
  let start_day = option None (digits <* char '/' >>| Option.return) in
  lift2 Tuple2.create (string "Poll! " *> start_day) (sep_by1 (char ',') one)

type opt =
  | A
  | B
  | C
  | D
  | E
  | F
[@@deriving sexp, enum, show { with_path = false }]

let emoji_of_opt = function
| A -> `Unicode_emoji "🇦"
| B -> `Unicode_emoji "🇧"
| C -> `Unicode_emoji "🇨"
| D -> `Unicode_emoji "🇩"
| E -> `Unicode_emoji "🇪"
| F -> `Unicode_emoji "🇫"

type day =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
[@@deriving sexp, enum, show { with_path = false }]

type parsed = {
  text: string;
  opts: opt list;
}
[@@deriving sexp]

let parse raw =
  Angstrom.parse_string parser ~consume:All raw
  |> Result.ok
  |> Option.map ~f:(fun (start_day, times) ->
         let start_day = Option.value_map start_day ~default:(day_to_enum Saturday) ~f:pred in
         let text, opts =
           List.foldi times ~init:([], 0) ~f:(fun i ((ll, n) as acc) -> function
             | None -> acc
             | Some x ->
               let opt = opt_of_enum n |> Option.value_exn ~here:[%here] in
               let s =
                 sprintf "%s: %s %s" (show_opt opt)
                   ((start_day + (i / 2)) % 7 |> day_of_enum |> Option.value_exn ~here:[%here] |> show_day)
                   x
               in
               (s, opt) :: ll, n + 1)
           |> fst
           |> List.rev
           |> List.unzip
           |> Tuple2.map_fst ~f:(String.concat ~sep:"\n")
         in
         { text; opts })

let%expect_test "D&D Poll Parser" =
  let test s = parse s |> [%sexp_of: parsed option] |> Sexp.to_string |> print_endline in
  test "Poll! 12,19,-,10";
  [%expect {| (((text"A: Saturday 12:00pm\nB: Saturday 7:00pm\nC: Sunday 10:00")(opts(A B C)))) |}];
  test "Poll! 7,19.,-,-";
  [%expect {| (((text"A: Saturday 7:00\nB: Saturday 7:30pm")(opts(A B)))) |}];
  test "Poll! 6/-,19,10.";
  [%expect {| (((text"A: Saturday 7:00pm\nB: Sunday 10:30")(opts(A B)))) |}];
  test "Poll! 7/-,19,10.";
  [%expect {| (((text"A: Sunday 7:00pm\nB: Monday 10:30")(opts(A B)))) |}];
  test "Poll 1/2/3/4";
  [%expect {| () |}];
  test "Poll! 4/10.,19.";
  [%expect {| (((text"A: Thursday 10:30\nB: Thursday 7:30pm")(opts(A B)))) |}];
  test "Poll! 5/10.,19.,13,-,-,14.,15";
  [%expect
    {| (((text"A: Friday 10:30\nB: Friday 7:30pm\nC: Saturday 1:00pm\nD: Sunday 2:30pm\nE: Monday 3:00pm")(opts(A B C D E)))) |}]

let on_message_create config = function
| Data.Message.{ id = _; type_ = DEFAULT; channel_id; content; author; _ }
 |Data.Message.{ id = _; type_ = REPLY; channel_id; content; author; _ }
  when Basics.Snowflake.equal author.id config.poll_user_id -> (
  match parse content with
  | Some { text; opts } ->
    let%lwt { id = message_id; _ } =
      Rest.Channel.create_message ~token:config.token ~channel_id ~content:text
    in
    Lwt_list.iter_s
      (fun opt ->
        let emoji = emoji_of_opt opt in
        Rest.Channel.create_reaction ~token:config.token ~channel_id ~message_id ~emoji)
      opts
  | None -> Lwt.return_unit
)
| _ -> Lwt.return_unit
