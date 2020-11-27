open! Core_kernel

open Router.Open

type close =
| Reconnecting
| Final
| Exception of Exn.t

let code_of_close = function
| Reconnecting -> 4000
| Final -> 1000
| Exception _ -> 4001

type event =
| Before_action of Message.t
| After_action of Message.t
| Before_reconnecting
| Error_connection_closed
| Error_connection_reset
| Error_connection_timed_out
| Error_discord_server of { extension: int; final: bool; content: string }
| Error_discontinuity of Internal_state.counter

type connection = {
  ic: Lwt_io.input_channel;
  oc: Lwt_io.output_channel;
  send: Websocket.Frame.t -> unit Lwt.t;
  cancel: Websocket.Frame.t Lwt.u;
}

type 'a action =
| Forward of 'a Router.Open.state
| Reconnect of (float option * 'a Router.Open.state)
| Exn of Exn.t

let forward state = Lwt.return (Forward state)
let reconnect ~wait state = Lwt.return (Reconnect (wait, state))
let exn_ exn = Lwt.return (Exn exn)

let in_background ~on_exn f = Lwt.async (fun () -> Lwt.catch f on_exn)

let shutdown = ref None
let make_shutdown send () =
  Lwt_main.at_exit (fun () ->
    Lwt_unix.with_timeout 1.0 (fun () ->
      code_of_close Final |> Websocket.Frame.close |> send
    ));
  raise Exit

let () =
  let handler _ = Option.call () ~f:!shutdown in
  let _sigint = Lwt_unix.on_signal Sys.sigint handler in
  let _sigterm = Lwt_unix.on_signal Sys.sigterm handler in
  ()

let handle_frame ~trigger_event login cancel send ({ internal_state; user_state } as state) frame =
  let open Websocket in
  begin match frame with
  | Frame.{ opcode = Ping; _ } ->
    let%lwt () = send @@ Frame.create ~opcode:Frame.Opcode.Pong () in
    Router.forward state

  | Frame.{ opcode = Close; extension; final; content } ->
    let%lwt user_state = trigger_event user_state (Error_discord_server { extension; final; content }) in
    Router.reconnect ~wait:None { internal_state; user_state }

  | Frame.{ opcode = Pong; _ } ->
    Router.forward state

  | Frame.{ opcode = Text; content; _ }
  | Frame.{ opcode = Binary; content; _ } ->
    let raw = Yojson.Safe.from_string content |> Protocol.Recv.of_yojson |> Result.ok_or_failwith in
    let message = Message.parse raw in
    let%lwt user_state = trigger_event user_state (Before_action message) in
    Internal_state.received_seq raw.s internal_state;
    begin match%lwt Router.handle_message login ~send ~cancel { internal_state; user_state } message with
    | R_Forward { internal_state; user_state } ->
      let%lwt user_state = trigger_event user_state (After_action message) in
      Router.forward { internal_state; user_state }
    | (R_Reconnect _ as x) -> Lwt.return x
    end

  | frame ->
    failwithf "Unhandled frame: %s" (Frame.show frame) ()
  end

let rec event_loop ~trigger_event login connection recv ({ internal_state; user_state } as state) =
  let%lwt action = Lwt.catch (fun () ->
      let%lwt frame = recv () in
      begin match%lwt handle_frame ~trigger_event login connection.cancel connection.send state frame with
      | R_Forward state -> forward state

      | R_Reconnect (wait, { internal_state; user_state }) ->
        Internal_state.terminate internal_state;
        let%lwt user_state = trigger_event user_state Before_reconnecting in
        reconnect ~wait { internal_state; user_state }
      end
    ) (fun exn ->
      Internal_state.terminate internal_state;
      let open Core in
      begin match exn with
      | End_of_file ->
        let%lwt user_state = trigger_event user_state Error_connection_closed in
        reconnect ~wait:None { internal_state; user_state }

      | Unix.Unix_error (Unix.ECONNRESET, _c, _s) ->
        let%lwt user_state = trigger_event user_state Error_connection_reset in
        reconnect ~wait:None { internal_state; user_state }

      | Unix.Unix_error (Unix.ETIMEDOUT, _c, _s) ->
        let%lwt user_state = trigger_event user_state Error_connection_timed_out in
        reconnect ~wait:None { internal_state; user_state }

      | Internal_state.Discontinuity_error counter ->
        let%lwt user_state = trigger_event user_state (Error_discontinuity counter) in
        reconnect ~wait:None { internal_state = Internal_state.create (); user_state }

      | exn -> exn_ exn
      end
    )
  in
  begin match action with
  | Forward state ->
    (event_loop [@tailcall]) ~trigger_event login connection recv state
  | x -> Lwt.return x
  end

let connect ~trigger_event (Login.{ token; _ } as login) state =
  let%lwt res = Rest.Gateway.bot ~token in
  let uri =
    begin match Uri.scheme res.url with
    | None
    | Some "wss" -> Uri.with_scheme res.url (Some "https")
    | Some "ws" -> Uri.with_scheme res.url (Some "http")
    | Some x -> failwithf "Invalid scheme in WS connect: %s" x ()
    end
    |> Fn.flip Uri.add_query_params ["v", ["8"]; "encoding", ["json"]]
  in

  let%lwt recv, send, ic, oc =
    let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
    let ctx = Conduit_lwt_unix.default_ctx in
    let%lwt client = Conduit_lwt_unix.endp_to_client ~ctx endp in
    Ws.with_connection ~ctx client uri
  in

  let cancel_p, cancel = Lwt.wait () in
  let cancelable_recv () = Lwt.choose [cancel_p; recv ()] in
  let send_timeout frame = Lwt_unix.with_timeout 1.0 (fun () -> send frame) in

  let connection = { ic; oc; send = send_timeout; cancel } in

  shutdown := Some (make_shutdown send_timeout);
  let%lwt action = event_loop ~trigger_event login connection cancelable_recv state in
  Lwt.return (action, connection)

let rec connection_loop ~trigger_event ~on_exn ~close_connection login blank_state previous_state =
  let%lwt state =
    let state = Option.value previous_state ~default:(blank_state ()) in
    begin match%lwt connect ~trigger_event login state with
    | Forward state, _ -> Lwt.return_some state

    | Reconnect (wait, state), connection ->
      let%lwt () = close_connection connection Reconnecting in
      let%lwt () = Option.value_map wait ~default:Lwt.return_unit ~f:Lwt_unix.sleep in
      Lwt.return_some state

    | Exn Exit, connection ->
      let%lwt () = close_connection connection Final in
      Lwt.return_none

    | Exn exn, connection ->
      let%lwt () =
        Backtrace.get ()
        |> Backtrace.to_string_list
        |> [%sexp_of: string list]
        |> Sexp.to_string
        |> Lwt_io.printlf "!!! %s"
      in
      let%lwt () = close_connection connection (Exception exn) in
      let%lwt () = on_exn exn in
      let%lwt () = Lwt_unix.sleep 5.0 in
      (* Reset the user state to avoid an infinite crash loop *)
      Lwt.return_some (blank_state ())
    end
  in
  begin match state with
  | (Some _ as state) ->
    (connection_loop [@tailcall]) ~trigger_event ~on_exn ~close_connection login blank_state state
  | None -> Lwt.return_unit
  end

module type S = sig
  type state

  val create : unit -> state
  val on_exn : exn -> unit Lwt.t
  val on_closing_connection : close -> unit Lwt.t
  val on_event : state -> event -> state Lwt.t
end

module Make (Bot : S) : sig
  val start : Login.t -> unit Lwt.t
end = struct

  let blank_state () = {
    internal_state = Internal_state.create ();
    user_state = Bot.create ();
  }

  let trigger_event user_state event =
    Lwt.catch (fun () -> Bot.on_event user_state event)
      Lwt.Infix.(function
      | Exit -> raise Exit
      | exn -> Bot.on_exn exn >|= fun () -> user_state
      )

  let close_connection { ic; oc; send; cancel = _ } close =
    let code = code_of_close close in
    let%lwt () = Bot.on_closing_connection close in
    let%lwt () =
      if Lwt_io.is_closed oc
      then Lwt.return_unit
      else
      Lwt.catch (fun () ->
        Lwt_unix.with_timeout 1.0 (fun () -> send @@ Websocket.Frame.close code))
        (fun _exn -> Lwt.return_unit)
    in
    Lwt_unix.with_timeout 1.0 (fun () -> Lwt.join [Lwt_io.close ic; Lwt_io.close oc])

  let start login = connection_loop
      ~trigger_event
      ~on_exn:Bot.on_exn
      ~close_connection
      login
      blank_state
      None

end
