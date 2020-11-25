open! Core_kernel

open Config

let parser =
  let open Angstrom in
  let digits = take_while1 (function
    | '0' .. '9' -> true
    | _ -> false
    ) >>| Int.of_string
  in
  let one =
    let dot = option false (char '.' *> return true) in
    let time = both digits dot >>| function
      | x, dot when x > 12 -> sprintf "%d:%d0pm" (x - 12) (if dot then 3 else 0)
      | x, dot -> sprintf "%d:%d0" x (if dot then 3 else 0)
    in
    (time >>| Option.return) <|> (char '-' *> return None)
  in
  let start_day = option None (digits <* char '/' >>| Option.return) in
  lift2 Tuple2.create (string "Poll! " *> start_day) (sep_by1 (char ',') one)

type letter = A | B | C | D [@@deriving sexp, enum, show { with_path = false }]
let emoji_of_letter = function
| A -> "🇦"
| B -> "🇧"
| C -> "🇨"
| D -> "🇩"

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
  letters: letter list;
} [@@deriving sexp]

let parse raw =
  Angstrom.parse_string parser ~consume:All raw
  |> Result.ok
  |> Option.map ~f:(fun (start_day, times) ->
    let start_day = Option.value_map start_day ~default:(day_to_enum Saturday) ~f:pred in
    let text, letters =
      List.foldi times ~init:([], 0) ~f:(fun i ((ll, n) as acc) -> function
      | None -> acc
      | Some x ->
        let letter = letter_of_enum n |> Option.value_exn ~here:[% here] in
        let s =
          sprintf "%s: %s %s"
            (show_letter letter)
            ((start_day + (i / 2)) % 7 |> day_of_enum |> Option.value_exn ~here:[% here] |> show_day)
            x
        in
        (s, letter) :: ll, n + 1
      )
      |> fst
      |> List.rev
      |> List.unzip
      |> Tuple2.map_fst ~f:(String.concat ~sep:"\n")
    in
    { text; letters }
  )

let%expect_test "Parser" =
  let test s = parse s |> [%sexp_of: parsed option] |> Sexp.to_string |> print_endline in
  test "Poll! 12,19,-,10";
  [%expect {| (((text"A: Saturday 12:00\nB: Saturday 7:00pm\nC: Sunday 10:00")(letters(A B C)))) |}];
  test "Poll! 7,19.,-,-";
  [%expect {| (((text"A: Saturday 7:00\nB: Saturday 7:30pm")(letters(A B)))) |}];
  test "Poll! 6/-,19,10.";
  [%expect {| (((text"A: Saturday 7:00pm\nB: Sunday 10:30")(letters(A B)))) |}];
  test "Poll! 7/-,19,10.";
  [%expect {| (((text"A: Sunday 7:00pm\nB: Monday 10:30")(letters(A B)))) |}];
  test "Poll 1/2/3/4";
  [%expect {| () |}];
  test "Poll! 4/10.,19.";
  [%expect {| (((text"A: Thursday 10:30\nB: Thursday 7:30pm")(letters(A B)))) |}]

let on_message_create config = function
| Objects.Message.{ id = _; type_ = DEFAULT; channel_id; content; author; _ } when String.(=) author.id config.poll_user_id ->
  begin match parse content with
  | Some { text; letters } ->
    let%lwt posted = Rest.Channel.create_message ~token:config.token ~channel_id ~content:text (Parse Objects.Message.of_yojson_exn) in
    Lwt_list.iter_s (fun letter ->
      let%lwt () = Latch.wait_and_trigger ~custom_cooldown:Int64.(2L * Latch.Time.sec) Rest.Call.latch in
      let emoji = emoji_of_letter letter in
      Rest.Channel.create_reaction ~token:config.token ~channel_id ~message_id:posted.id ~emoji Ignore
    ) letters
  | None -> Lwt.return_unit
  end
| _ -> Lwt.return_unit
