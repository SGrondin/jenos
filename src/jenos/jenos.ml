open! Core_kernel
open Discord

let create_bot config =
  Random.set_state (Random.State.make_self_init ());
  let module Bot = Bot.Make (struct
    open Bot

    type state = { vc_state: Track_vc.state }

    let create () = { vc_state = Track_vc.initial_state }

    let ( >>> ) f x = Lwt.Infix.(f >|= fun () -> x)

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
      Lwt_io.printlf "⚠️ Received a Close frame. extension: %d. final: %b. content: %s" extension
        final
        Sexp.(to_string (Atom content))
      >>> state
    | Error_discontinuity { count; ack } ->
      Lwt_io.printlf "❌ Discontinuity error: ACK = %d but COUNT = %d. Closing the connection" ack count
      >>> state
    | Before_connecting gateway ->
      Lwt_io.printl (Rest.Gateway.sexp_of_t gateway |> Sexp.to_string) >>> state
    | Before_reconnecting -> Lwt_io.printl "🌐 Reconnecting..." >>> state
    (* READY *)
    | Msg (Ready _) ->
      Lwt_io.printl "✅ READY!" >>> { vc_state = { vc_state with just_started = false } }
    (* RECONNECT *)
    | Msg Reconnect -> Lwt_io.printl "⚠️ Received a Reconnect request." >>> state
    (* RESUMED *)
    | Msg Resumed -> Lwt_io.printl "▶️ Resumed" >>> state
    (* INVALID_SESSION *)
    | Msg (Invalid_session { resumable }) ->
      Lwt_io.printlf "⚠️ Session rejected (resumable: %b), starting a new session..." resumable
      >>> state
    (* VOICE_STATE_UPDATE *)
    | Msg (Voice_state_update vsu) ->
      let%lwt tracker = Track_vc.on_voice_state_update config vc_state vsu in
      Lwt.return { vc_state = { vc_state with tracker } }
    (* GUILD_CREATE *)
    | Msg (Guild_create gc) ->
      let%lwt tracker = Track_vc.on_guild_create config vc_state gc in
      Lwt.return { vc_state = { vc_state with tracker } }
    (* MESSAGE_CREATE *)
    | Msg (Message_create message) ->
      in_background ~on_exn (fun () ->
          Lwt.join
            [
              Make_poll.on_message_create config message;
              Make_meeting_poll.on_message_create config message;
              Add_reactions.on_message_create config message;
            ]);
      Lwt.return state
    (* | Payload ({ op = Dispatch; _ } as payload) -> *)
    (* Lwt_io.printl ([%sexp_of: Data.Payload.t] payload |> Sexp.to_string_hum) >>> state *)
    (* Re-test *)
    (* | Msg (Invite_create x) ->
         Lwt_io.printlf "Invite_create!!! %s"
           ([%sexp_of: Data.Events.Invite_create.t] x |> Sexp.to_string_hum)
         >>> state
       | Msg (Invite_delete x) ->
         Lwt_io.printlf "Invite_delete!!! %s"
           ([%sexp_of: Data.Events.Invite_delete.t] x |> Sexp.to_string_hum)
         >>> state *)
    (* Testing *)
    (* Other events *)
    | _ -> Lwt.return state
  end) in
  Bot.start
