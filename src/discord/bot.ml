open! Core_kernel

type respond = Message.Send.t -> unit Lwt.t

module type S = sig
  type t [@@deriving sexp]

  val initial : unit -> t

  val before_reidentifying : t -> t Lwt.t
  val before_reconnecting : t -> t Lwt.t
  val on_connection_closed : unit -> unit Lwt.t
  val on_exn : exn -> unit Lwt.t

  val before_handler : respond:respond -> t -> Message.Recv.t -> t Lwt.t
  val after_handler : respond:respond -> t -> Message.Recv.t -> t Lwt.t
end

module Make (Bot : S) : sig
  val start : Config.t -> unit Lwt.t
end = struct

  let shutdown = ref None
  let make_shutdown send () =
    Lwt_main.at_exit (fun () -> Router.close_timeout ~code:1000 send);
    raise Exit

  let () =
    let handler _ = Option.call () ~f:!shutdown in
    let _sigint = Lwt_unix.on_signal Sys.sigint handler in
    let _sigterm = Lwt_unix.on_signal Sys.sigterm handler in
    ()

  let handle_frame config send (Router.{ internal_state; user_state } as state) frame =
    let open Websocket in
    begin match frame with
    | Frame.{ opcode = Ping; _ } ->
      let%lwt () = send @@ Frame.create ~opcode:Frame.Opcode.Pong () in
      Lwt.return state

    | Frame.{ opcode = Close; extension; final; content } ->
      let%lwt () =
        Lwt_io.eprintlf "⚠️ Received a Close frame. extension: %d. final: %b. content: %s"
          extension final Sexp.(to_string (Atom content))
      in
      raise (Router.Reconnect (internal_state, Bot.sexp_of_t user_state))

    | Frame.{ opcode = Pong; _ } ->
      Lwt.return state

    | Frame.{ opcode = Text; content; _ }
    | Frame.{ opcode = Binary; content; _ } ->
      let message = Yojson.Safe.from_string content |> Message.Recv.of_yojson_exn in
      Internal_state.received_seq message.s internal_state;
      Router.handle_message config send state message
        ~before_handler:Bot.before_handler
        ~after_handler:Bot.after_handler
        ~user_state_to_sexp:Bot.sexp_of_t
        ~on_exn:Bot.on_exn

    | frame ->
      failwithf "Unhandled frame: %s" (Frame.show frame) ()
    end

  (* The event loop is the only place that can close connections except for the heartbeat *)
  let rec event_loop config recv send (Router.{ internal_state; user_state } as state) =
    let%lwt updated = Lwt.catch (fun () ->
        let%lwt frame = recv () in
        handle_frame config send state frame
      ) (function
      | Router.Reidentify (internal_state, sexp) ->
        let user_state = Bot.t_of_sexp sexp in
        let%lwt user_state = Bot.before_reidentifying user_state in
        let%lwt () = Lwt_unix.sleep (Random.float_range 1.0 5.0) in
        let%lwt () = Router.identify config send in
        Lwt.return Router.{ internal_state; user_state }

      | Router.Reconnect (internal_state, sexp) ->
        Internal_state.terminate internal_state;
        let user_state = Bot.t_of_sexp sexp in
        let%lwt user_state = Bot.before_reconnecting user_state in
        let%lwt () = Router.close_timeout ~code:1002 send in
        raise (Router.Reconnect (internal_state, Bot.sexp_of_t user_state))

      | End_of_file ->
        Internal_state.terminate internal_state;
        let%lwt () = Bot.on_connection_closed () in
        raise (Router.Reconnect (internal_state, Bot.sexp_of_t user_state))

      | exn ->
        Internal_state.terminate internal_state;
        let%lwt () = Router.close_timeout ~code:1001 send in
        raise exn
      )
    in
    event_loop config recv send updated

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

  let blank_state ?(user_state = Bot.initial ()) () =
    Router.{
      internal_state = Internal_state.initial ();
      user_state;
    }

  let rec connection_loop (config : Config.t) state =
    let%lwt res = Rest.Gateway.get ~token:config.token in
    Lwt.catch (fun () -> connect config state res.url)
      (function
      | Router.Reconnect (internal_state, sexp) ->
        let user_state = Bot.t_of_sexp sexp in
        connection_loop config Router.{ internal_state; user_state }
      | exn ->
        let%lwt () = Bot.on_exn exn in
        let%lwt () = Lwt_unix.sleep 5.0 in
        connection_loop config (blank_state ())
      )

  let start config = connection_loop config (blank_state ())

end
