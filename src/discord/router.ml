open! Core_kernel
open Websocket

exception Reconnect

let paladins_vc = "641490017134575637"
let paladins_text = "563523694316486670"

let every ~f ms =
  let rec loop s =
    let%lwt () = Lwt_unix.sleep s in
    Lwt.async (fun () ->
      Lwt.catch f (fun exn -> Lwt_io.eprintlf "Background loop exn: %s" (Exn.to_string exn))
    );
    loop s
  in
  Lwt.async (fun () -> loop (ms // 1000))

let tracker = String.Table.create ()

let handle send : Message.Recv.t -> unit Lwt.t = function
| { op = Hello; d; _ } ->
  let hello = Events.Hello.of_yojson_exn d in
  let f () =
    Commands.Heartbeat.to_message !Message.heartbeat_seq
    |> Message.respond send
  in
  every hello.heartbeat_interval ~f;
  Commands.Identify.{
    token = Rest.token;
    properties = {
      os = "Linux";
      browser = Rest.name;
      device = Rest.name;
    };
    compress = false;
    presence = {
      (* since = Option.return Commands.t0; *)
      since = None;
      activities = Some [{
          name = ":dark_sunglasses: Killin' it";
          type_ = 4;
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
  |> Message.respond send

| { op = Heartbeat; _ } ->
  Commands.Heartbeat_ACK.to_message ()
  |> Message.respond send

| { op = Heartbeat_ACK; _ } ->
  Lwt.return_unit

| { op = Invalid_session; _ } ->
  let%lwt () = Lwt_io.printl "Shutting down..." in
  send @@ Frame.close 1000

| { op = Dispatch; t = Some "READY"; s = _; d } ->
  let ready = Events.Ready.of_yojson_exn d in
  Lwt_io.printlf "READY! %s" ([%sexp_of: Events.Ready.t] ready |> Sexp.to_string)

| { op = Dispatch; t = Some "VOICE_STATE_UPDATE"; s = _; d } ->
  let vsu = Events.Voice_state_update.of_yojson_exn d in
  let key = vsu.user_id in
  let nick = Option.bind vsu.member ~f:(fun x -> x.nick) |> Option.value ~default:"❓" in
  let%lwt () = begin match vsu with
  (* Paladins 1 *)
  | { channel_id = Some channel_id; _ } when String.(=) channel_id paladins_vc ->
    String.Table.set tracker ~key ~data:vsu;
    Lwt_io.printlf "📈 %s" nick
  (* Disconnected or joined another channel *)
  | _ ->
    begin match String.Table.find_and_remove tracker key with
    | None -> Lwt.return_unit
    | Some _ -> Lwt_io.printlf "📉 %s" nick
    end
  end
  in
  begin match String.Table.length tracker with
  | 0 | 1 -> Lwt.return_unit
  | _ ->
    let%lwt () = Post.send ~channel_id:paladins_text ~content:"Oh shiiiit looks like the party's getting started 🎉" in
    Lwt_io.printlf "Posted to <%s>" paladins_text
  end

| { op = Dispatch; _ } ->
  Lwt_io.printl "!!! Unhandled dispatch"

| { op = Reconnect; _ } ->
  raise Reconnect

| ({ op = Identify; _ } as x)
| ({ op = Presence_update; _ } as x)
| ({ op = Voice_state_update; _ } as x)
| ({ op = Resume; _ } as x)
| ({ op = Request_guild_members; _ } as x) ->
  failwithf "Unsupported opcode: %s" ([%sexp_of: Message.Recv.t] x |> Sexp.to_string) ()
