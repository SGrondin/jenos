open! Core_kernel
open Config

let regex = Regex.create {|https://discord.com/channels/[0-9]{5,}/([0-9]{5,})/([0-9]{5,})\s+([a-z]+)|}

let sos = Discord.Data.Basics.Snowflake.of_string

let on_message_create config = function
| Data.Message.{ type_ = REPLY; content; _ }
 |Data.Message.{ type_ = DEFAULT; content; _ } -> (
  match Regex.exec regex content with
  | Some [| _whole; channel_id; message_id; word |] ->
    String.to_list word
    |> List.stable_dedup
    |> List.map ~f:Make_poll.emoji_of_letter
    |> Add_reactions.add_emojis ~token:config.token ~channel_id:(sos channel_id)
         ~message_id:(sos message_id)
  | Some _
   |None ->
    Lwt.return_unit
)
| _ -> Lwt.return_unit

let%expect_test "Capture 3 ids and a word" =
  Regex.exec regex
    "https://discord.com/channels/448249875805634572/563523694316486670/800495738525319179 something"
  |> [%sexp_of: string array option]
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect
    {|
    (("https://discord.com/channels/448249875805634572/563523694316486670/800495738525319179 something"
      563523694316486670 800495738525319179 something)) |}]
