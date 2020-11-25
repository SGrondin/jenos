open! Core_kernel

open Config

let on_message_create config = function
| Objects.Message.{ id = message_id; type_ = DEFAULT; channel_id; content; _ } ->
  let emojis =
    List.filter_map config.reactions ~f:(function
    | { regex; emojis } when Regex.matching regex content -> Some emojis
    | _ -> None
    )
    |> List.concat_no_order
  in
  Lwt_list.iter_s (fun emoji ->
    Rest.Channel.create_reaction ~token:config.token ~channel_id ~message_id ~emoji Ignore
  ) emojis
| _ -> Lwt.return_unit
