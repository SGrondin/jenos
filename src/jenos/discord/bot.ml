open! Core_kernel

type close =
| Reconnecting
| Final
| Exception of Exn.t

type event =
| Before_action of Message.Recv.t
| After_action of Message.Recv.t
| Before_reidentifying
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

let in_background ~on_exn f = Lwt.async (fun () -> Lwt.catch f on_exn)

module type S = sig
  type t

  val create : unit -> t
  val on_exn : exn -> unit Lwt.t
  val on_connection_closed : close -> unit Lwt.t
  val on_event : t -> event -> t Lwt.t
end

module Make (Bot : S) : sig
  val start : Login.t -> unit Lwt.t
end = struct

  open Router.Open

  exception Restart of (connection * Bot.t state)

  let code_of_close = function
  | Reconnecting -> 4000
  | Final -> 1000
  | Exception _ -> 4001

  let restart connection state = raise (Restart (connection, state))

  let blank_state ?(user_state = Bot.create ()) () = {
    internal_state = Internal_state.create ();
    user_state;
  }

  let run_handler user_state event =
    Lwt.catch (fun () -> Bot.on_event user_state event)
      Lwt.Infix.(fun exn -> Bot.on_exn exn >|= fun () -> user_state)

  let close_connection { ic; oc; send; cancel = _ } close =
    let code = code_of_close close in
    let%lwt () = Bot.on_connection_closed close in
    let%lwt () =
      if Lwt_io.is_closed oc
      then Lwt.return_unit
      else
      Lwt.catch (fun () ->
        Lwt_unix.with_timeout 1.0 (fun () -> send @@ Websocket.Frame.close code))
        (fun _exn -> Lwt.return_unit)
    in
    Lwt_unix.with_timeout 1.0 (fun () -> Lwt.join [Lwt_io.close ic; Lwt_io.close oc])

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

  let handle_frame login cancel send ({ internal_state; user_state } as state) frame =
    let open Websocket in
    begin match frame with
    | Frame.{ opcode = Ping; _ } ->
      let%lwt () = send @@ Frame.create ~opcode:Frame.Opcode.Pong () in
      forward state

    | Frame.{ opcode = Close; extension; final; content } ->
      let%lwt user_state = run_handler user_state (Error_discord_server { extension; final; content }) in
      reconnect { internal_state; user_state }

    | Frame.{ opcode = Pong; _ } ->
      forward state

    | Frame.{ opcode = Text; content; _ }
    | Frame.{ opcode = Binary; content; _ } ->
      let message = Yojson.Safe.from_string content |> Message.Recv.of_yojson_exn in
      let%lwt user_state = run_handler user_state (Before_action message) in
      Internal_state.received_seq message.s internal_state;
      begin match%lwt Router.handle_message login ~send ~cancel { internal_state; user_state } message with
      | Forward { internal_state; user_state } ->
        let%lwt user_state = run_handler user_state (After_action message) in
        forward { internal_state; user_state }
      | x -> Lwt.return x
      end

    | frame ->
      failwithf "Unhandled frame: %s" (Frame.show frame) ()
    end

  let rec event_loop login connection recv ({ internal_state; user_state } as state) =
    let%lwt state = Lwt.catch (fun () ->
        let%lwt frame = recv () in
        begin match%lwt handle_frame login connection.cancel connection.send state frame with
        | Forward state -> Lwt.return state

        | Reidentify { internal_state; user_state } ->
          let%lwt user_state = run_handler user_state Before_reidentifying in
          let%lwt () = Lwt_unix.sleep (Random.float_range 1.0 5.0) in
          let%lwt () = Router.identify login connection.send in
          Lwt.return { internal_state; user_state }

        | Reconnect { internal_state; user_state } ->
          Internal_state.terminate internal_state;
          let%lwt user_state = run_handler user_state Before_reconnecting in
          restart connection { internal_state; user_state }
        end
      ) (fun exn ->
        Internal_state.terminate internal_state;
        let open Core in
        begin match exn with
        | End_of_file ->
          let%lwt user_state = run_handler user_state Error_connection_closed in
          restart connection { internal_state; user_state }

        | Unix.Unix_error (Unix.ECONNRESET, _c, _s) ->
          let%lwt user_state = run_handler user_state Error_connection_reset in
          restart connection { internal_state; user_state }

        | Unix.Unix_error (Unix.ETIMEDOUT, _c, _s) ->
          let%lwt user_state = run_handler user_state Error_connection_timed_out in
          restart connection { internal_state; user_state }

        | Internal_state.Discontinuity_error counter ->
          let%lwt user_state = run_handler user_state (Error_discontinuity counter) in
          restart connection (blank_state ~user_state ())

        | (Restart _ as exn) -> raise exn

        | Exit ->
          let%lwt () = close_connection connection Final in
          raise Exit

        | exn ->
          let%lwt () =
            Backtrace.get ()
            |> Backtrace.to_string_list
            |> [%sexp_of: string list]
            |> Sexp.to_string
            |> Lwt_io.printlf "!!! %s"
          in
          let%lwt () = close_connection connection (Exception exn) in
          raise exn
        end
      )
    in
    event_loop login connection recv state

  let connect login state uri =
    let uri =
      begin match Uri.scheme uri with
      | None
      | Some "wss" -> Uri.with_scheme uri (Some "https")
      | Some "ws" -> Uri.with_scheme uri (Some "http")
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
    event_loop login connection cancelable_recv state

  let rec connection_loop (login : Login.t) state =
    let%lwt res = Rest.Gateway.get ~token:login.token in
    let%lwt state = Lwt.catch (fun () ->
        connect login state res.url
      ) (function
      | Restart (connection, state) ->
        let%lwt () = close_connection connection Reconnecting in
        Lwt.return state
      | Exit -> raise Exit
      | exn ->
        let%lwt () = Bot.on_exn exn in
        let%lwt () = Lwt_unix.sleep 5.0 in
        Lwt.return (blank_state ())
      )
    in
    connection_loop login state

  let start login = connection_loop login (blank_state ())

end
