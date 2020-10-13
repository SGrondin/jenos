open! Core_kernel
open Websocket

let vc_channel = Env.get "VC_CHANNEL"
let text_channel = Env.get "TEXT_CHANNEL"
let line2 = Env.get "LINE2"
let line4 = Env.get "LINE4"

type vc_change =
(* Voice State Update *)
| VSU of Channel.member option
(* Guild Create *)
| GC of Channel.member list

let vc_member_change ~before ~after change =
  let nick_opt opt =
    Option.bind opt ~f:Channel.member_name
    |> Option.value ~default:"❓"
  in
  let nick member =
    Channel.member_name member
    |> Option.value ~default:"❓"
  in
  let%lwt () = begin match change with
  | VSU member when before < after -> Lwt_io.printlf "📈 %s" (nick_opt member)
  | VSU member when before > after -> Lwt_io.printlf "📉 %s" (nick_opt member)
  | VSU _
  | GC [] -> Lwt.return_unit
  | GC ll ->
    List.map ll ~f:(fun m -> sprintf "⚡ %s" (nick m))
    |> String.concat ~sep:"\n"
    |> Lwt_io.printl
  end
  in
  let%lwt () = Lwt_io.printlf "Current state: %d member%s" (after) (if after > 1 then "s" else "") in
  if after > before
  then begin match after with
  | 2 ->
    let%lwt () = Post.send ~channel_id:text_channel ~content:line2 in
    Lwt_io.printlf "✅ Posted to <%s>" text_channel
  | 4 ->
    let%lwt () = Post.send ~channel_id:text_channel ~content:line4 in
    Lwt_io.printlf "✅ Posted to <%s>" text_channel
  | _ -> Lwt.return_unit
  end
  else Lwt.return_unit

let handle send state : Message.Recv.t -> State.t Lwt.t = function
| { op = Hello; d; _ } ->
  let hello = Events.Hello.of_yojson_exn d in
  let%lwt () =
    begin match State.session_id state with
    | Some id ->
      Commands.Resume.{
        token = Rest.token;
        session_id = id;
        seq = !(State.seq state);
      }
      |> Commands.Resume.to_message
    | None ->
      Commands.Identify.{
        token = Rest.token;
        properties = {
          os = "Linux";
          browser = Rest.name;
          device = Rest.name;
        };
        compress = false;
        presence = {
          since = None;
          activities = Some [
              Commands.Identify.Game "Party Manager"
            ];
          status = "online";
          afk = false;
        };
        guild_subscriptions = false;
        (* GUILD_VOICE_STATES (1 << 7) and GUILDS (1 << 0) *)
        intents = 1 lsl 7 + 1 lsl 0;
      }
      |> Commands.Identify.to_message
    end
    |> Message.respond send
  in
  let heartbeat_interval = hello.heartbeat_interval in
  State.received_hello ~heartbeat_interval send state
  |> Lwt.return

| { op = Heartbeat; _ } ->
  let%lwt () = Commands.Heartbeat_ACK.to_message |> Message.respond send in
  Lwt.return state

| { op = Heartbeat_ACK; _ } ->
  Lwt.return state

| { op = Invalid_session; _ } ->
  let%lwt () = Lwt_io.printl "Shutting down..." in
  let%lwt () = send @@ Frame.close 1000 in
  raise State.Reconnect

| { op = Dispatch; t = Some "READY"; s = _; d } ->
  let ready = Events.Ready.of_yojson_exn d in
  let%lwt () = Lwt_io.printlf "READY! %s" ([%sexp_of: Events.Ready.t] ready |> Sexp.to_string) in
  State.received_ready state ~session_id:ready.session_id
  |> Lwt.return

| { op = Dispatch; t = Some "VOICE_STATE_UPDATE"; s = _; d } ->
  let vsu = Events.Voice_state_update.of_yojson_exn d in
  let key = vsu.user_id in
  let before = State.size state in
  let state = begin match vsu with
  | { channel_id = Some channel_id; _ } when String.(=) channel_id vc_channel ->
    (* Target channel *)
    State.Voice.track_user ~key state
  | _ ->
    (* Disconnected or joined another channel *)
    State.Voice.forget_user ~key state
  end
  in
  let after = State.size state in
  let%lwt () = vc_member_change ~before ~after (VSU vsu.member) in
  Lwt.return state

| { op = Dispatch; t = Some "GUILD_CREATE"; s = _; d } ->
  let before = State.size state in
  let gc = Events.Guild_create.of_yojson_exn d in
  let users_in_target_channel = begin match gc with
  | { voice_states = None; _ } -> String.Set.empty
  | { voice_states = Some ll; _ } ->
    List.filter_map ll ~f:(function
    | { channel_id = Some channel_id; user_id; _ } when String.(=) channel_id vc_channel ->
      Some user_id
    | _ -> None
    )
    |> String.Set.of_list
  end
  in
  let ids, members = begin match gc.members with
  | None -> String.Set.empty, []
  | Some members ->
    List.fold members ~init:(String.Set.empty, []) ~f:(fun ((set, ll) as acc) -> function
    | (Channel.{ user = Some { id; _ }; _ } as member)
      when String.Set.mem users_in_target_channel id ->
      if String.Set.mem set id
      then acc
      else (String.Set.add set id, (member :: ll))
    | _ -> acc
    )
  end
  in
  let state = State.Voice.replace_all ids state in
  let after = State.size state in
  let%lwt () = vc_member_change ~before ~after (GC members) in
  Lwt.return state

| { op = Dispatch; t = Some "RESUMED"; s = _; d = _ } ->
  let%lwt () = Lwt_io.printl "⏯️ Resumed" in
  Lwt.return state

| { op = Dispatch; _ } ->
  let%lwt () = Lwt_io.printl "Unhandled dispatch" in
  Lwt.return state

| { op = Reconnect; _ } ->
  raise (State.Resume state)

| ({ op = Identify; _ } as x)
| ({ op = Presence_update; _ } as x)
| ({ op = Voice_state_update; _ } as x)
| ({ op = Resume; _ } as x)
| ({ op = Request_guild_members; _ } as x) ->
  failwithf "Unsupported opcode: %s" ([%sexp_of: Message.Recv.t] x |> Sexp.to_string) ()
