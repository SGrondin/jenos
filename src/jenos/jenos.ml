open! Core_kernel
open Discord

let create_bot config =
  Random.set_state (Random.State.make_self_init ());
  let module Bot = Bot.Make (struct
    open Bot

    type state = {
      vc_state: Track_vc.state;
      curses_state: Send_curses.state;
      remaining: int option;
    }

    let create () =
      {
        vc_state = Track_vc.initial_state;
        curses_state = Send_curses.initial_state Config.(config.send_curses);
        remaining = None;
      }

    let ( >>> ) f x = Lwt.Infix.(f >|= fun () -> x)

    let on_exn exn = Lwt_io.printlf "❌ Unexpected error: %s" (Exn.to_string exn)

    let on_closing_connection = function
    | Final -> Lwt_io.printl "⏹️ Closing connection (Final)..."
    | Exception exn -> Lwt_io.printlf "⏹️ Closing connection (Exception %s)..." (Exn.to_string exn)
    | Reconnecting -> Lwt_io.printl "⏹️ Closing connection (Reconnecting)..."

    let on_event ({ vc_state; _ } as state) = function
    | Error_connection_closed -> Lwt_io.printl "🔌 Connection was closed." >>> state
    | Error_connection_reset -> Lwt_io.printl "❗ Connection was reset." >>> state
    | Error_connection_timed_out -> Lwt_io.printl "⏱️ Connection timed out." >>> state
    | Error_discord_server { extension; final; content } ->
      Lwt_io.printlf "⚠️ Received a Close frame. extension: %d. final: %b. content: %s" extension
        final
        Sexp.(to_string (Atom content))
      >>> state
    | Error_discontinuity { count; ack } ->
      Lwt_io.printlf "❌ Discontinuity error: ACK = %d but COUNT = %d. Closing the connection" ack count
      >>> state
    | Before_connecting gateway ->
      let remaining = gateway.session_start_limit.remaining in
      Lwt.return { state with remaining = Some remaining }
    | Before_reconnecting -> Lwt_io.printl "🌐 Reconnecting..." >>> state
    (* READY *)
    | Received (Ready _) -> Lwt_io.printl "✅ READY!" >>> state
    (* RECONNECT *)
    | Received Reconnect -> Lwt_io.printl "⚠️ Received a Reconnect request." >>> state
    (* RESUMED *)
    | Received Resumed ->
      let remaining = Option.value_map state.remaining ~default:"?" ~f:Int.to_string in
      Lwt_io.printlf "▶️ Resumed (%s)" remaining >>> state
    (* INVALID_SESSION *)
    | Received (Invalid_session { resumable }) ->
      Lwt_io.printlf "⚠️ Session rejected (resumable: %b), starting a new session..." resumable
      >>> state
    (* VOICE_STATE_UPDATE *)
    | Received (Voice_state_update vsu) ->
      let%lwt tracker = Track_vc.on_voice_state_update config vc_state vsu in
      Lwt.return { state with vc_state = { vc_state with tracker } }
    (* GUILD_CREATE *)
    | Received (Guild_create gc) ->
      let%lwt tracker = Track_vc.on_guild_create config vc_state gc in
      Lwt.return { state with vc_state = { vc_state with tracker } }
    (* MESSAGE_CREATE *)
    | Received (Message_create message) ->
      in_background ~on_exn (fun () ->
          Lwt.join
            [
              Make_poll.on_message_create config message;
              Make_meeting_poll.on_message_create config message;
              Add_reactions.on_message_create config message;
              Add_reactions_to_reply.on_message_create config message;
            ]);
      let%lwt curses_state =
        Send_curses.on_message_create ~in_background:(in_background ~on_exn) config state.curses_state
          message
      in
      Lwt.return { state with curses_state }
    (* | Payload ({ op = Dispatch; _ } as payload) -> *)
    (* Lwt_io.printl ([%sexp_of: Data.Payload.t] payload |> Sexp.to_string_hum) >>> state *)
    (* Re-test *)
    | Received (Invite_create x) ->
      Lwt_io.printlf "Invite_create!!! %s"
        ([%sexp_of: Data.Events.Invite_create.t] x |> Sexp.to_string_hum)
      >>> state
    | Received (Invite_delete x) ->
      Lwt_io.printlf "Invite_delete!!! %s"
        ([%sexp_of: Data.Events.Invite_delete.t] x |> Sexp.to_string_hum)
      >>> state
    (* Testing *)
    (* Other events *)
    | _ -> Lwt.return state
  end) in
  Bot.start
