open! Core_kernel
open Websocket

exception Reconnect of (bool * Internal_state.t * Sexp.t)
exception Reidentify of (Internal_state.t * Sexp.t)

type 'a state = {
  internal_state: Internal_state.t;
  user_state: 'a;
}

let close_timeout ?(timeout = 1.0) ~code send =
  Lwt_unix.with_timeout timeout (fun () ->
    let%lwt () = Lwt_io.printl "⏹️ Closing connection..." in
    send @@ Frame.close code
  )

let run_handler handler ~on_exn ~respond user_state msg =
  Lwt.catch (fun () -> handler ~respond user_state msg)
    (fun exn ->
        let%lwt () = on_exn exn in
        Lwt.return user_state
    )

let send_response send message =
  let content = Message.Send.to_yojson message |> Yojson.Safe.to_string in
  Frame.create ~opcode:Frame.Opcode.Text ~content ()
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

let handle_message
    ~user_state_to_sexp ~before_handler ~after_handler ~on_exn
    (config : Config.t) send { internal_state; user_state } (msg : Message.Recv.t) =

  let respond = send_response send in
  (* BEFORE_HANDLER *)
  let%lwt user_state = run_handler before_handler ~on_exn ~respond user_state msg in
  (* HANDLER *)
  let%lwt updated_internal_state = begin match msg with
  | { op = Hello; d; _ } ->
    let hello = Events.Hello.of_yojson_exn d in
    let%lwt () =
      let open Commands in
      begin match Internal_state.session_id internal_state with
      | Some id ->
        (* Resume session *)
        Resume.{
          token = config.token;
          session_id = id;
          seq = Internal_state.seq internal_state;
        }
        |> Resume.to_message
        |> respond
      | None ->
        (* New session *)
        identify config send
      end
    in
    let heartbeat_interval = hello.heartbeat_interval in
    let close ack count =
      let%lwt () = Lwt_io.printlf "❌ Discontinuity error: ACK = %d but HB = %d. Closing the connection" ack count in
      close_timeout ~code:1002 send
    in
    Internal_state.received_hello ~heartbeat_interval respond close internal_state
    |> Lwt.return

  | { op = Heartbeat; _ } ->
    let%lwt () = Commands.Heartbeat_ACK.to_message |> respond in
    Lwt.return internal_state

  | { op = Heartbeat_ACK; _ } ->
    Internal_state.received_ack internal_state;
    Lwt.return internal_state

  | { op = Invalid_session; _ } ->
    raise (Reidentify (internal_state, user_state_to_sexp user_state))

  | { op = Dispatch; t = Some "READY"; s = _; d } ->
    let ready = Events.Ready.of_yojson_exn d in
    Internal_state.received_ready ~session_id:ready.session_id internal_state
    |> Lwt.return

  | { op = Dispatch; _ } ->
    Lwt.return internal_state

  | { op = Reconnect; d; _ } ->
    let reconnect = Events.Reconnect.of_yojson_exn d in
    raise (Reconnect (reconnect, internal_state, user_state_to_sexp user_state))

  | ({ op = Identify; _ } as x)
  | ({ op = Presence_update; _ } as x)
  | ({ op = Voice_state_update; _ } as x)
  | ({ op = Resume; _ } as x)
  | ({ op = Request_guild_members; _ } as x) ->
    failwithf "Unexpected opcode: %s. Please report this bug." ([%sexp_of: Message.Recv.t] x |> Sexp.to_string) ()
  end
  in
  (* AFTER_HANDLER *)
  let%lwt user_state = run_handler after_handler ~on_exn ~respond user_state msg in
  Lwt.return {
    internal_state = updated_internal_state;
    user_state;
  }
