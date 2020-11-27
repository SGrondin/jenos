open! Core_kernel

open Discord

let create_bot config =
  Random.set_state (Random.State.make_self_init ());
  let module Bot = Bot.Make (struct
      open Bot
      type state = {
        vc_state: Track_vc.state;
      }

      let create () = {
        vc_state = Track_vc.initial_state;
      }

      let (>>>) f x = Lwt.map (fun () -> x) f

      let on_exn exn = Lwt_io.printlf "❌ Unexpected error: %s" (Exn.to_string exn)

      let on_closing_connection = function
      | Final -> Lwt_io.printl "⏹️ Closing connection (Final)..."
      | Exception exn -> Lwt_io.printlf "⏹️ Closing connection (Exception %s)..." (Exn.to_string exn)
      | Reconnecting -> Lwt_io.printl "⏹️ Closing connection (Reconnecting)..."

      let on_event ({ vc_state } as state) = function
      | Error_connection_closed -> Lwt_io.printl "🔌 Connection was closed." >>> state
      | Error_connection_reset -> Lwt_io.printl "❗ Connection was reset." >>> state
      | Error_connection_timed_out -> Lwt_io.printl "⏱️ Connection timed out." >>> state
      | Error_discord_server { extension; final; content } ->
        Lwt_io.printlf "⚠️ Received a Close frame. extension: %d. final: %b. content: %s"
          extension final Sexp.(to_string (Atom content))
        >>> state
      | Error_discontinuity { count; ack } ->
        Lwt_io.printlf "❌ Discontinuity error: ACK = %d but COUNT = %d. Closing the connection" ack count
        >>> state
      | Before_reconnecting -> Lwt_io.printl "🌐 Reconnecting..." >>> state

      (* READY *)
      | Before_action { parsed = Ready _; raw = { d; _ } } ->
        Lwt_io.printlf "✅ READY! %s" (Yojson.Safe.to_string d)
        >>> { vc_state = { vc_state with just_started = false }}

      (* RECONNECT *)
      | Before_action { parsed = Reconnect; _ } ->
        Lwt_io.printl "⚠️ Received a Reconnect request." >>> state

      (* RESUMED *)
      | Before_action { parsed = Resumed; _ } ->
        Lwt_io.printl "▶️ Resumed" >>> state

      (* INVALID_SESSION *)
      | Before_action { parsed = Invalid_session { resumable }; _ } ->
        Lwt_io.printlf "⚠️ Session rejected (resumable: %b), starting a new session..." resumable >>> state


      (* VOICE_STATE_UPDATE *)
      | After_action { parsed = Voice_state_update vsu; _ } ->
        let%lwt tracker = Track_vc.on_voice_state_update config vc_state vsu in
        Lwt.return { vc_state = { vc_state with tracker } }

      (* GUILD_CREATE *)
      | After_action { parsed = Guild_create gc; _ } ->
        let%lwt tracker = Track_vc.on_guild_create config vc_state gc in
        Lwt.return { vc_state = { vc_state with tracker } }

      (* MESSAGE_CREATE *)
      | After_action { parsed = Message_create message; _ } ->
        in_background ~on_exn (fun () ->
          Lwt.join [
            Make_poll.on_message_create config message;
            Make_meeting_poll.on_message_create config message;
            Add_reactions.on_message_create config message;
          ]
        );
        Lwt.return state

      (* Other events *)
      | _ -> Lwt.return state
    end)
  in
  Bot.start
