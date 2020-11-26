open! Core_kernel

open Discord

let create_bot config =
  Random.set_state (Random.State.make_self_init ());
  let module Bot = Bot.Make (struct
      open Bot
      type t = {
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
      | Before_reidentifying -> Lwt_io.printl "⏯️ Resuming..." >>> state
      | Before_reconnecting -> Lwt_io.printl "🌐 Reconnecting..." >>> state
      | Before_action msg -> begin match msg with
        (* READY *)
        | { op = Dispatch; t = Some "READY"; s = _; d } ->
          Lwt_io.printlf "✅ READY! %s" (Yojson.Safe.to_string d)
          >>> { vc_state = { vc_state with just_started = false }}

        (* RECONNECT *)
        | { op = Reconnect; _ } ->
          Lwt_io.printl "⚠️ Received a Reconnect request." >>> state

        (* RESUMED *)
        | { op = Dispatch; t = Some "RESUMED"; s = _; d = _ } ->
          Lwt_io.printl "▶️ Resumed" >>> state

        (* INVALID_SESSION *)
        | { op = Invalid_session; _ } ->
          Lwt_io.printl "⚠️ Session rejected, starting a new session..." >>> state

        | _ -> Lwt.return state
        end

      | After_action msg -> begin match msg with
        (* VOICE_STATE_UPDATE *)
        | { op = Dispatch; t = Some "VOICE_STATE_UPDATE"; s = _; d } ->
          let vsu = Events.Voice_state_update.of_yojson_exn d in
          let%lwt tracker = Track_vc.on_voice_state_update config vc_state vsu in
          Lwt.return { vc_state = { vc_state with tracker } }

        (* GUILD_CREATE *)
        | { op = Dispatch; t = Some "GUILD_CREATE"; s = _; d } ->
          let gc = Events.Guild_create.of_yojson_exn d in
          let%lwt tracker = Track_vc.on_guild_create config vc_state gc in
          Lwt.return { vc_state = { vc_state with tracker } }

        (* MESSAGE_CREATE *)
        | { op = Dispatch; t = Some "MESSAGE_CREATE"; s = _; d } ->
          let message = Objects.Message.of_yojson_exn d in
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
        end
    end)
  in
  Bot.start
