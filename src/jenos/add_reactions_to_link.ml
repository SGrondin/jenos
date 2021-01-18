open! Core_kernel
open Config

let regex = Regex.create {|^!([a-z]+$)|}

let on_message_create config = function
| Data.Message.{ type_ = REPLY; content; referenced_message = Some { id = message_id; channel_id; _ }; _ }
  -> (
  match Regex.exec regex content with
  | Some [| _whole; word |] ->
    String.to_list word
    |> List.stable_dedup
    |> List.map ~f:Make_poll.emoji_of_letter
    |> Add_reactions.add_emojis ~token:config.token ~channel_id ~message_id
  | Some _
   |None ->
    Lwt.return_unit
)
| _ -> Lwt.return_unit

let%expect_test "Capture the word" =
  Regex.exec regex "!something" |> [%sexp_of: string array option] |> Sexp.to_string_hum |> print_endline;
  [%expect {|
    ((!something something)) |}]
