open! Core_kernel

module Open = struct
  type 'a state = {
    internal_state: Internal_state.t;
    user_state: 'a;
  }

  type 'a action =
  | Forward of 'a state
  | Reconnect of 'a state
  | Reidentify of 'a state

  let forward state = Lwt.return (Forward state)
  let reconnect state = Lwt.return (Reconnect state)
  let reidentify state = Lwt.return (Reidentify state)
end

open Open

let send_response send message =
  let content = Message.Send.to_yojson message |> Yojson.Safe.to_string in
  Websocket.Frame.create ~opcode:Text ~content ()
  |> send

let identify (config : Config.t) send =
  let open Commands in
  Identify.{
    token = config.token;
    properties = {
      os = "UNIX";
      browser = Rest.Call.name;
      device = Rest.Call.name;
    };
    compress = false;
    presence = {
      since = None;
      activities = Some [config.activity];
      status = config.status;
      afk = config.afk;
    };
    guild_subscriptions = false;
    intents = config.intents;
  }
  |> Identify.to_message
  |> send_response send

let resume (config : Config.t) send internal_state id =
  let open Commands in
  Resume.{
    token = config.token;
    session_id = id;
    seq = Internal_state.seq internal_state;
  }
  |> Resume.to_message
  |> send_response send

let handle_message (config : Config.t) ~send ~cancel ({ internal_state; user_state } as state) = function
| Message.Recv.{ op = Hello; d; _ } ->
  let hello = Events.Hello.of_yojson_exn d in
  let%lwt () = begin match Internal_state.session_id internal_state with
  | Some id -> resume config send internal_state id
  | None -> identify config send
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
