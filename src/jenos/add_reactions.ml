open! Core_kernel
open Config

let on_message_create config = function
| Data.Message.{ id = message_id; type_ = REPLY; channel_id; content; _ }
 |Data.Message.{ id = message_id; type_ = DEFAULT; channel_id; content; _ } ->
  let emojis =
    List.filter_map config.reactions ~f:(function
      | { matcher; emojis } when Regex.matching matcher content -> Some emojis
      | _ -> None)
    |> List.concat_no_order
  in
  Lwt_list.iter_s
    (fun emoji -> Rest.Channel.create_reaction ~token:config.token ~channel_id ~message_id ~emoji)
    emojis
| _ -> Lwt.return_unit
