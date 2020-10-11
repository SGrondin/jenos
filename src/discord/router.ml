open! Core_kernel
open Websocket

let vc_channel = Env.get "VC_CHANNEL"
let text_channel = Env.get "TEXT_CHANNEL"
let threshold = 2

let handle send state : Message.Recv.t -> State.t Lwt.t = function
| { op = Hello; d; _ } ->
  let hello = Events.Hello.of_yojson_exn d in
  let%lwt () =
    begin match State.session_id state with
    | Some id ->
      Commands.Resume.({ session_id = id; seq = !(State.seq state) } |> to_message)
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
          activities = Some [{
              type_ = Commands.Identify.Custom {
                  emoji = "dark_sunglasses";
                  name = "party manager";
                };
              created_at = Commands.t0;
            }];
          status = "online";
          afk = false;
        };
        guild_subscriptions = false;
        (* GUILD_VOICE_STATES (1 << 7) and GUILD_MESSAGES (1 << 9) *)
        intents = 1 lsl 7;
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
  let nick =
    let open Option.Monad_infix in
    Option.first_some
      (vsu.member >>= Events.Voice_state_update.nick)
      (vsu.member >>= Events.Voice_state_update.user >>| User.username)
    |> Option.value ~default:"❓"
  in
  let before = State.size state in
  let%lwt state = begin match vsu with
  | { channel_id = Some channel_id; _ } when String.(=) channel_id vc_channel ->
    (* Target channel *)
    State.track_user ~key ~data:vsu state |> Lwt.return
  | _ ->
    (* Disconnected or joined another channel *)
    State.forget_user ~key state |> Lwt.return
  end
  in
  let after = State.size state in
  let%lwt () =
    if before < after then Lwt_io.printlf "📈 %s" nick
    else if before > after then  Lwt_io.printlf "📉 %s" nick
    else Lwt.return_unit
  in
  let%lwt () =
    if before < threshold && after >= threshold
    then begin
      let%lwt () = Post.send ~channel_id:text_channel ~content:"Oh shiiiit looks like the party's getting started 🎉" in
      Lwt_io.printlf "✅ Posted to <%s>" text_channel
    end
    else Lwt.return_unit
  in
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
