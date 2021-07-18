open! Core_kernel
open Config
open Lwt.Syntax

let ( *= ) = [%equal: Basics.Snowflake.t]

let ( >>> ) f x = Lwt.Infix.(f >>= fun () -> x)

module SMap = Basics.Snowflake.Map

let mutex = Lwt_mutex.create ()

let apply_roles ~token ~channel_id ~message_id ~emoji ~guild_id ~role_id =
  let* must_have =
    let+ users = Rest.Channel.get_reactions ~token ~channel_id ~message_id ~emoji in
    List.fold users ~init:SMap.empty ~f:(fun acc user -> SMap.set acc ~key:user.id ~data:())
  in
  let* actually_have =
    let+ members = Rest.Guild.list_guild_members ~token ~guild_id ~limit:1000 () in
    List.fold members ~init:SMap.empty ~f:(fun acc -> function
      | { user = Some { id; _ }; roles; _ } when List.mem roles ~equal:( *= ) role_id ->
        SMap.set acc ~key:id ~data:()
      | _ -> acc)
  in
  SMap.fold2 must_have actually_have ~init:Lwt.return_unit ~f:(fun ~key:user_id ~data acc ->
      let* () = acc in
      match data with
      | `Both ((), ()) -> Lwt.return_unit
      | `Left () ->
        Lwt_io.printlf !"Granting role <%{Basics.Snowflake}> to <%{Basics.Snowflake}>" role_id user_id
        >>> Rest.Guild.add_guild_member_role ~token ~guild_id ~user_id ~role_id
      | `Right () ->
        Lwt_io.printlf !"Removing role <%{Basics.Snowflake}> from <%{Basics.Snowflake}>" role_id user_id
        >>> Rest.Guild.remove_guild_member_role ~token ~guild_id ~user_id ~role_id)

type t =
  [ `Add          of Data.Events.Message_reaction_add.t
  | `Remove       of Data.Events.Message_reaction_remove.t
  | `Remove_emoji of Data.Events.Message_reaction_remove_emoji.t
  ]

let on_message_reaction { token; role_react; _ } : t -> unit Lwt.t = function
| `Add { channel_id; message_id; guild_id = Some guild_id; emoji = { name = Some emoji; _ }; _ }
 |`Remove { channel_id; message_id; guild_id = Some guild_id; emoji = { name = Some emoji; _ }; _ }
 |`Remove_emoji { channel_id; message_id; guild_id = Some guild_id; emoji = { name = Some emoji; _ }; _ }
  when channel_id *= role_react.channel_id -> (
  match String.Map.find role_react.grants emoji with
  | None -> Lwt.return_unit
  | Some role_id -> (
    let* message = Rest.Channel.get_channel_message ~token ~channel_id ~message_id in
    match message with
    | { author = { id = author_id; _ }; _ } when author_id *= role_react.from_user_id ->
      Lwt_mutex.with_lock mutex (fun () ->
          apply_roles ~token ~channel_id ~message_id ~emoji:(`Unicode_emoji emoji) ~guild_id ~role_id)
    | _ -> Lwt.return_unit
  )
)
| _ -> Lwt.return_unit
