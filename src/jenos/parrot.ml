open! Core_kernel
open Lwt.Syntax
open Config

let on_message_create { token; parrot; _ } = function
| Data.Message.
    { id = message_id; type_ = DEFAULT; channel_id; content; guild_id; author = { id = author; _ }; _ } ->
  List.find parrot ~f:(fun { matcher; user_id; _ } ->
      Option.value_map user_id ~default:true ~f:(Basics.Snowflake.equal author)
      && Regex.matching matcher content)
  |> Option.value_map ~default:Lwt.return_unit ~f:(fun { say = content; _ } ->
         let message_reference : Data.Message.Reference.t =
           {
             message_id = Some message_id;
             channel_id = Some channel_id;
             guild_id;
             fail_if_not_exists = Some true;
           }
         in
         let+ _msg = Rest.Channel.create_message ~token ~channel_id ~content ~message_reference () in
         ())
| _ -> Lwt.return_unit
