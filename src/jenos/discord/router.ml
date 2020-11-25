open! Core_kernel

module Open = struct
  type 'a state = {
    internal_state: Internal_state.t;
    user_state: 'a;
  }

  type 'a router_action =
  | R_Forward of 'a state
  | R_Reconnect of 'a state
  | R_Reidentify of 'a state
end

open Open

let forward state = Lwt.return (R_Forward state)
let reconnect state = Lwt.return (R_Reconnect state)
let reidentify state = Lwt.return (R_Reidentify state)

let send_response send message =
  let content = Message.Send.to_yojson message |> Yojson.Safe.to_string in
  Websocket.Frame.create ~opcode:Text ~content ()
  |> send

let identify ({ token; activity; status; afk; intents } : Login.t) send =
  {
    token;
    properties = {
      os = "UNIX";
      browser = Rest.Call.name;
      device = Rest.Call.name;
    };
    compress = false;
    presence = {
      since = None;
      activities = Some [activity];
      status;
      afk;
    };
    guild_subscriptions = false;
    intents;
  }
  |> Commands.Identify.to_message
  |> send_response send

let resume ({ token; _ } : Login.t) send internal_state session_id =
  {
    token;
    session_id;
    seq = Internal_state.seq internal_state;
  }
  |> Commands.Resume.to_message
  |> send_response send

let handle_message (login : Login.t) ~send ~cancel ({ internal_state; user_state } as state) = function
| Message.Recv.{ op = Hello; d; _ } ->
  let hello = Events.Hello.of_yojson_exn d in
  let%lwt () = begin match Internal_state.session_id internal_state with
  | Some id -> resume login send internal_state id
  | None -> identify login send
  end
  in
  let heartbeat_loop = Internal_state.{
      interval = hello.heartbeat_interval;
      respond = send_response send;
      cancel;
    }
  in
  let internal_state = Internal_state.received_hello heartbeat_loop internal_state in
  forward { internal_state; user_state }

| { op = Heartbeat; _ } ->
  let%lwt () = Commands.Heartbeat_ACK.to_message |> send_response send in
  forward state

| { op = Heartbeat_ACK; _ } ->
  Internal_state.received_ack internal_state;
  forward state

| { op = Invalid_session; d; _ } ->
  if Events.Invalid_session.of_yojson_exn d
  then reidentify state
  else reconnect state

| { op = Dispatch; t = Some "READY"; s = _; d } ->
  let Events.Ready.{ session_id; _ } = Events.Ready.of_yojson_exn d in
  let internal_state = Internal_state.received_ready ~session_id internal_state in
  forward { internal_state; user_state }

| { op = Dispatch; _ } ->
  forward state

| { op = Reconnect; _ } ->
  reconnect state

| ({ op = Identify; _ } as x)
| ({ op = Presence_update; _ } as x)
| ({ op = Voice_state_update; _ } as x)
| ({ op = Resume; _ } as x)
| ({ op = Request_guild_members; _ } as x) ->
  failwithf "Unexpected opcode: %s. Please report this bug." ([%sexp_of: Message.Recv.t] x |> Sexp.to_string) ()
