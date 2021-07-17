open! Core_kernel

let kick_inactive ({ token; _ } : Jenos__.Config.t) =
  let open Jenos__.Config in
  let module Snowflake = Discord.Data.Basics.Snowflake in
  let sos = Snowflake.of_string in
  let%lwt checked =
    let%lwt users =
      Discord.Rest.Channel.get_reactions ~token ~channel_id:(sos "563530163522895882")
        ~message_id:(sos "854889445919555614") ~emoji:(`Unicode_emoji "✅")
    in
    List.fold users ~init:Snowflake.Map.empty ~f:(fun acc user ->
        Snowflake.Map.add_exn acc ~key:user.id ~data:user.username)
    |> Lwt.return
  in
  let guild_id = sos "448249875805634572" in
  let%lwt cutting =
    let cutting_board_role_id = sos "854136441235701770" in
    let%lwt members = Discord.Rest.Guild.list_guild_members ~token ~guild_id ~limit:160 () in
    List.fold members ~init:Snowflake.Map.empty ~f:(fun acc -> function
      | member when List.mem member.roles ~equal:[%equal: Snowflake.t] cutting_board_role_id ->
        let user = member.user |> Option.value_exn ~here:[%here] in
        Snowflake.Map.add_exn acc ~key:user.id ~data:user.username
      | _ -> acc)
    |> Lwt.return
  in

  let kick =
    Snowflake.Map.merge cutting checked ~f:(fun ~key:_ -> function
      | `Left x -> Some x
      | `Right _
       |`Both _ ->
        None)
  in

  print_endline (sprintf !"Kicking %d members..." (Snowflake.Map.length kick));

  let%lwt () =
    Snowflake.Map.fold kick ~init:Lwt.return_unit ~f:(fun ~key ~data acc ->
        let%lwt () = acc in
        print_endline (sprintf !"Kicking %s (%{Snowflake})" data key);
        Discord.Rest.Guild.remove_guild_member ~token ~guild_id ~user_id:key)
  in
  print_endline "Done";
  Lwt.return_unit
