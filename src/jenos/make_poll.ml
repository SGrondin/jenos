open! Core_kernel

open Config

type choice = {
  opt: char;
  text: string;
} [@@deriving sexp]

type poll = {
  question: string;
  options: choice list;
} [@@deriving sexp]

let emoji_of_opt = function
| 'A' -> "🇦"
| 'B' -> "🇧"
| 'C' -> "🇨"
| 'D' -> "🇩"
| 'E' -> "🇪"
| 'F' -> "🇫"
| 'G' -> "🇬"
| 'H' -> "🇭"
| 'I' -> "🇮"
| 'J' -> "🇯"
| 'K' -> "🇰"
| 'L' -> "🇱"
| 'M' -> "🇲"
| 'N' -> "🇳"
| 'O' -> "🇴"
| 'P' -> "🇵"
| 'Q' -> "🇶"
| 'R' -> "🇷"
| 'S' -> "🇸"
| 'T' -> "🇹"
| 'U' -> "🇺"
| 'V' -> "🇻"
| 'W' -> "🇼"
| 'X' -> "🇽"
| 'Y' -> "🇾"
| 'Z' -> "🇿"
| c -> failwithf "Invalid option '%c'" c ()

let parser =
  let open Angstrom in
  let opt = satisfy (function
    | 'A' .. 'Z' | 'a' .. 'z' -> true
    | _ -> false
    ) >>| Char.uppercase
  in
  let ws = skip_while (Char.(=) ' ') in
  let rest = take_till (Char.(=) '\n') >>| String.strip in
  let line = lift2 (fun opt text -> { opt; text })
      (ws *> char '-' *> ws *> opt <* ws <* char ':')
      rest
  in
  lift2 (fun question options -> { question; options })
    (ws *> string_ci "Poll!" *> rest <* char '\n' <* skip_while (Char.(<>) '-'))
    (sep_by1 end_of_line line)

let parse raw =
  Angstrom.parse_string parser ~consume:Prefix raw
  |> Result.ok
  |> Option.map ~f:(fun poll ->
    List.fold_until poll.options ~init:Char.Set.empty ~f:(fun acc { opt; _ } ->
      if Char.Set.mem acc opt
      then Stop (Error (sprintf "Duplicate option '%c'" opt))
      else Continue (Char.Set.add acc opt)
    )
      ~finish:(fun set ->
        if Char.Set.length set < 2
        then Error "There should be at least 2 choices"
        else Ok poll
      )
  )

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
  [%expect {| ((Ok((question"super duper poll!")(options(((opt A)(text blah))((opt B)(text bleh))((opt C)(text ok!))))))) |}];
  test {|Poll! This is a question

-A: blah
- B: bleh
  |};
  [%expect {| ((Ok((question"This is a question")(options(((opt A)(text blah))((opt B)(text bleh))))))) |}];
  ()

let on_message_create { token; _ } = function
| Objects.Message.{ id = message_id; type_ = DEFAULT; channel_id; content; _ } ->
  begin match parse content with
  | Some (Error msg) ->
    Rest.Channel.create_message ~token ~channel_id ~content:(sprintf ":x: %s" msg) Ignore

  | Some (Ok poll) ->
    let%lwt () = Rest.Channel.delete_message ~token ~channel_id ~message_id in
    let buf = Buffer.create 64 in
    Buffer.add_string buf poll.question;
    List.iter poll.options ~f:(fun { opt; text } ->
      bprintf buf "\n%s %s" (emoji_of_opt opt) text
    );
    let content = Buffer.contents buf in
    let%lwt post = Rest.Channel.create_message ~token ~channel_id ~content (Parse Objects.Message.of_yojson) in
    Lwt_list.iter_s (fun { opt; _ } ->
      Rest.Channel.create_reaction ~token ~channel_id ~message_id:post.id ~emoji:(emoji_of_opt opt) Ignore
    ) poll.options

  | None -> Lwt.return_unit
  end
| _ -> Lwt.return_unit
