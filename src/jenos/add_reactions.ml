open! Core_kernel
open Config

let add_emojis ~token ~channel_id ~message_id emojis =
  Lwt_list.iter_s (fun emoji -> Rest.Channel.create_reaction ~token ~channel_id ~message_id ~emoji) emojis

let on_message_create { token; add_reactions; _ } = function
| Data.Message.{ id = message_id; type_ = REPLY; channel_id; content; _ }
 |Data.Message.{ id = message_id; type_ = DEFAULT; channel_id; content; _ } ->
  let emojis =
    List.filter_map add_reactions ~f:(function
      | { matcher; emojis } when Regex.matching matcher content -> Some emojis
      | _ -> None)
    |> List.concat_no_order
  in
  add_emojis ~token ~channel_id ~message_id emojis
| _ -> Lwt.return_unit
