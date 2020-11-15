open! Core_kernel

type close =
| Reconnecting
| Final
| Unexpected

type background_event =
| Closing_connection of close
| Discontinuity_error of { heartbeat: int; ack: int}

type lifecycle_event =
| Before_action of Message.Recv.t
| After_action of Message.Recv.t
| Before_reidentifying
| Before_reconnecting
| Connection_lost
| Discord_websocket_issue of { extension: int; final: bool; content: string }

let in_background ~on_exn f = Lwt.async (fun () -> Lwt.catch f on_exn)

module type S = sig
  type t

  val create : unit -> t
  val on_exn : exn -> unit Lwt.t
  val on_background_event : background_event -> unit Lwt.t
  val on_lifecycle_event : t -> lifecycle_event -> t Lwt.t
end

module Make (Bot : S) : sig
  val start : Config.t -> unit Lwt.t
end = struct

  open Router.Open

  let run_handler user_state event =
    Lwt.catch (fun () -> Bot.on_lifecycle_event user_state event)
      (fun exn -> Bot.on_exn exn >>> user_state)

  let close_timeout ?(timeout = 1.0) send close =
    let code = begin match close with
    | Reconnecting -> 1002
    | Final -> 1000
    | Unexpected -> 1001
    end
    in
    let%lwt () = Bot.on_background_event (Closing_connection close) in
    Lwt_unix.with_timeout timeout (fun () ->
      send @@ Websocket.Frame.close code
    )

  let shutdown = ref None
  let make_shutdown send () =
    Lwt_main.at_exit (fun () -> close_timeout send Final);
    raise Exit

  let () =
    let handler _ = Option.call () ~f:!shutdown in
    let _sigint = Lwt_unix.on_signal Sys.sigint handler in
    let _sigterm = Lwt_unix.on_signal Sys.sigterm handler in
    ()

  let handle_frame config send ({ internal_state; user_state } as state) frame =
    let open Websocket in
    begin match frame with
    | Frame.{ opcode = Ping; _ } ->
      let%lwt () = send @@ Frame.create ~opcode:Frame.Opcode.Pong () in
      forward state

    | Frame.{ opcode = Close; extension; final; content } ->
      let%lwt user_state = Bot.on_lifecycle_event user_state (Discord_websocket_issue { extension; final; content }) in
      reconnect { internal_state; user_state }

    | Frame.{ opcode = Pong; _ } ->
      forward state

    | Frame.{ opcode = Text; content; _ }
    | Frame.{ opcode = Binary; content; _ } ->
      let message = Yojson.Safe.from_string content |> Message.Recv.of_yojson_exn in
      let%lwt user_state = run_handler user_state (Before_action message) in
      Internal_state.received_seq message.s internal_state;
      let close ~ack ~count =
        let%lwt () = Bot.on_background_event (Discontinuity_error { heartbeat = count; ack }) in
        close_timeout send Reconnecting
      in
      begin match%lwt Router.handle_message config ~send ~close ~on_exn:Bot.on_exn { internal_state; user_state } message with
      | Forward { internal_state; user_state } ->
        let%lwt user_state = run_handler user_state (After_action message) in
        forward { internal_state; user_state }
      | x -> Lwt.return x
      end

    | frame ->
      failwithf "Unhandled frame: %s" (Frame.show frame) ()
    end

  (* The event loop is the only place that can close connections except for the heartbeat *)
  let rec event_loop config recv send ({ internal_state; user_state } as state) =
    Lwt.catch (fun () ->
      let%lwt frame = recv () in
      begin match%lwt handle_frame config send state frame with
      | Forward state ->
        event_loop config recv send state

      | Reidentify { internal_state; user_state } ->
        let%lwt user_state = Bot.on_lifecycle_event user_state Before_reidentifying in
        let%lwt () = Lwt_unix.sleep (Random.float_range 1.0 5.0) in
        let%lwt () = Router.identify config send in
        event_loop config recv send { internal_state; user_state }

      | Reconnect { internal_state; user_state } ->
        Internal_state.terminate internal_state;
        let%lwt user_state = Bot.on_lifecycle_event user_state Before_reconnecting in
        close_timeout send Reconnecting
        >>> { internal_state; user_state }
      end
    ) (function
    | End_of_file ->
      Internal_state.terminate internal_state;
      let%lwt user_state = Bot.on_lifecycle_event user_state Connection_lost in
      Lwt.return { internal_state; user_state }

    | exn ->
      Internal_state.terminate internal_state;
      let%lwt () = close_timeout send Unexpected in
      raise exn
    )

  let connect config state uri =
    let uri =
      begin match Uri.scheme uri with
      | None
      | Some "wss" -> Uri.with_scheme uri (Some "https")
      | Some "ws" -> Uri.with_scheme uri (Some "http")
      | Some x -> failwithf "Invalid scheme in WS connect: %s" x ()
      end
      |> Fn.flip Uri.add_query_params ["v", ["8"]; "encoding", ["json"]]
    in

    let%lwt recv, send =
      let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
      let ctx = Conduit_lwt_unix.default_ctx in
      let%lwt client = Conduit_lwt_unix.endp_to_client ~ctx endp in
      Websocket_lwt_unix.with_connection ~ctx client uri
    in

    shutdown := Some (make_shutdown send);
    event_loop config recv send state

  let blank_state ?(user_state = Bot.create ()) () = {
    internal_state = Internal_state.create ();
    user_state;
  }

  let rec connection_loop (config : Config.t) state =
    let%lwt res = Rest.Gateway.get ~token:config.token in
    Lwt.catch (fun () ->
      let%lwt state = connect config state res.url in
      connection_loop config state
    ) (fun exn ->
      let%lwt () = Bot.on_exn exn in
      let%lwt () = Lwt_unix.sleep 5.0 in
      connection_loop config (blank_state ())
    )

  let start config = connection_loop config (blank_state ())

end
