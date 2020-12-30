open! Core_kernel
open Config

type choice = {
  opt: char;
  text: string;
}
[@@deriving sexp]

type poll = {
  question: string;
  options: choice list;
}
[@@deriving sexp]

let emoji_of_opt = function
| 'A' -> `Unicode_emoji "🇦"
| 'B' -> `Unicode_emoji "🇧"
| 'C' -> `Unicode_emoji "🇨"
| 'D' -> `Unicode_emoji "🇩"
| 'E' -> `Unicode_emoji "🇪"
| 'F' -> `Unicode_emoji "🇫"
| 'G' -> `Unicode_emoji "🇬"
| 'H' -> `Unicode_emoji "🇭"
| 'I' -> `Unicode_emoji "🇮"
| 'J' -> `Unicode_emoji "🇯"
| 'K' -> `Unicode_emoji "🇰"
| 'L' -> `Unicode_emoji "🇱"
| 'M' -> `Unicode_emoji "🇲"
| 'N' -> `Unicode_emoji "🇳"
| 'O' -> `Unicode_emoji "🇴"
| 'P' -> `Unicode_emoji "🇵"
| 'Q' -> `Unicode_emoji "🇶"
| 'R' -> `Unicode_emoji "🇷"
| 'S' -> `Unicode_emoji "🇸"
| 'T' -> `Unicode_emoji "🇹"
| 'U' -> `Unicode_emoji "🇺"
| 'V' -> `Unicode_emoji "🇻"
| 'W' -> `Unicode_emoji "🇼"
| 'X' -> `Unicode_emoji "🇽"
| 'Y' -> `Unicode_emoji "🇾"
| 'Z' -> `Unicode_emoji "🇿"
| c -> failwithf "Invalid option '%c'" c ()

let parser =
  let open Angstrom in
  let opt =
    satisfy (function
      | 'A' .. 'Z'
       |'a' .. 'z' ->
        true
      | _ -> false)
    >>| Char.uppercase
  in
  let ws = skip_while (Char.( = ) ' ') in
  let rest = take_till (Char.( = ) '\n') >>| String.strip in
  let line = lift2 (fun opt text -> { opt; text }) (ws *> char '-' *> ws *> opt <* ws <* char ':') rest in
  lift2
    (fun question options -> { question; options })
    (ws *> string_ci "Poll!" *> rest <* char '\n' <* skip_while (Char.( <> ) '-'))
    (sep_by1 end_of_line line)

let parse raw =
  Angstrom.parse_string parser ~consume:Prefix raw
  |> Result.ok
  |> Option.map ~f:(fun poll ->
         List.fold_until poll.options ~init:Char.Set.empty
           ~f:(fun acc { opt; _ } ->
             if Char.Set.mem acc opt
             then Stop (Error (sprintf "Duplicate option '%c'" opt))
             else Continue (Char.Set.add acc opt))
           ~finish:(fun set ->
             if Char.Set.length set < 2 then Error "There should be at least 2 choices" else Ok poll))

let%expect_test "Poll Parser" =
  let test s = parse s |> [%sexp_of: (poll, string) Result.t option] |> Sexp.to_string |> print_endline in
  test {|Poll! - A: blah|};
  [%expect {| () |}];
  test {|Poll!
  - A: blah|};
  [%expect {| ((Error"There should be at least 2 choices")) |}];
  test {|Poll!
  - A: blah
  - A: bleh
  |};
  [%expect {| ((Error"Duplicate option 'A'")) |}];
  test {|Poll! super duper poll!
  - A: blah
  - B: bleh
  -C:ok!
  |};
  [%expect
    {| ((Ok((question"super duper poll!")(options(((opt A)(text blah))((opt B)(text bleh))((opt C)(text ok!))))))) |}];
  test {|Poll! This is a question

-A: blah
- B: bleh
  |};
  [%expect
    {| ((Ok((question"This is a question")(options(((opt A)(text blah))((opt B)(text bleh))))))) |}];
  ()

let on_message_create { token; _ } = function
| Data.Message.{ id = message_id; type_ = DEFAULT; channel_id; content; _ }
 |Data.Message.{ id = message_id; type_ = REPLY; channel_id; content; _ } -> (
  match parse content with
  | Some (Error msg) ->
    let%lwt _message = Rest.Channel.create_message ~token ~channel_id ~content:(sprintf "❌ %s" msg) in
    Lwt.return_unit
  | Some (Ok poll) ->
    let%lwt () = Rest.Channel.delete_message ~token ~channel_id ~message_id in
    let buf = Buffer.create 64 in
    Buffer.add_string buf poll.question;
    List.iter poll.options ~f:(fun { opt; text } ->
        bprintf buf "\n%s %s" (emoji_of_opt opt |> Basics.Reference.to_string) text);
    let content = Buffer.contents buf in
    let%lwt { id = message_id; _ } = Rest.Channel.create_message ~token ~channel_id ~content in
    Lwt_list.iter_s
      (fun { opt; _ } ->
        Rest.Channel.create_reaction ~token ~channel_id ~message_id ~emoji:(emoji_of_opt opt))
      poll.options
  | None -> Lwt.return_unit
)
| _ -> Lwt.return_unit
