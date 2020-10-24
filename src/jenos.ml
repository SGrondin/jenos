open! Core_kernel

open Discord

type config = {
  token: string;
  status: Commands.Identify.activity option [@default None];
  line2: string;
  line4: string;
  vc_channel: string;
  text_channel: string;
} [@@deriving of_yojson { exn = true }, show]

type vc_change =
(* Voice State Update *)
| VSU of Channel.member option
(* Guild Create *)
| GC of Channel.member list

let previous = ref (Int64.of_int 0)
let cooldown = Int64.(20L * 60L * 1_000_000_000L)

let can_send () =
  let open Int64 in
  let now = Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64 in
  let b = now > (!previous + cooldown) in
  if b then previous := now;
  b

let post_message ~token ~channel_id ~content =
  if can_send ()
  then Rest.Channel.create_message ~token ~channel_id ~content
  else Lwt_io.printl "⏳ Waiting until we can send again"

let vc_member_change { token; text_channel; line2; line4; _ } ~before ~after change =
  let nick_opt opt =
    Option.bind opt ~f:Channel.member_name
    |> Option.value ~default:"❓"
  in
  let nick member =
    Channel.member_name member
    |> Option.value ~default:"❓"
  in
  let%lwt () = begin match change with
  | VSU member when before < after -> Lwt_io.printlf "📈 %s" (nick_opt member)
  | VSU member when before > after -> Lwt_io.printlf "📉 %s" (nick_opt member)
  | VSU _
  | GC [] -> Lwt.return_unit
  | GC ll ->
    List.map ll ~f:(fun m -> sprintf "⚡ %s" (nick m))
    |> String.concat ~sep:"\n"
    |> Lwt_io.printl
  end
  in
  let%lwt () =
    if after <> before
    then Lwt_io.printlf "Current count: %d" after
    else Lwt.return_unit
  in
  if after > before
  then begin match after with
  | 2 ->
    let%lwt () = post_message ~token ~channel_id:text_channel ~content:line2 in
    Lwt_io.printlf "✅ Posted to <%s>" text_channel
  | 4 ->
    let%lwt () = post_message ~token ~channel_id:text_channel ~content:line4 in
    Lwt_io.printlf "✅ Posted to <%s>" text_channel
  | _ -> Lwt.return_unit
  end
  else Lwt.return_unit

let create_bot config =
  let module Bot = Bot.Make (struct
      type t = {
        tracker: String.Set.t;
      } [@@deriving sexp]

      let initial () = { tracker = String.Set.empty }

      let before_resuming () = Lwt_io.printl "⏯️ Resuming..."
      let before_reconnecting () = Lwt_io.printl "🌐 Reconnecting..."
      let on_connection_closed () = Lwt_io.eprintl "🔌 Connection was closed."
      let on_exn exn = Lwt_io.eprintlf "❌ Unexpected error: %s" (Exn.to_string exn)

      let before_handler ~respond:_ state : Message.Recv.t -> t Lwt.t = function
      (* READY *)
      | { op = Dispatch; t = Some "READY"; s = _; d } ->
        let%lwt () = Lwt_io.printlf "READY! %s" (Yojson.Safe.to_string d) in
        Lwt.return state

      (* RECONNECT *)
      | { op = Reconnect; _ } ->
        let%lwt () = Lwt_io.printl "⚠️ Received a Reconnect event." in
        Lwt.return state

      (* RESUMED *)
      | { op = Dispatch; t = Some "RESUMED"; s = _; d = _ } ->
        let%lwt () = Lwt_io.printl "⏯️ Resumed" in
        Lwt.return state

      (* INVALID_SESSION *)
      | { op = Invalid_session; _ } ->
        let%lwt () = Lwt_io.eprintl "🔌 Session rejected, starting a new session..." in
        Lwt.return state

      | _ -> Lwt.return state

      let after_handler ~respond:_ ({ tracker } as state) : Message.Recv.t -> t Lwt.t = function
      (* VOICE_STATE_UPDATE *)
      | { op = Dispatch; t = Some "VOICE_STATE_UPDATE"; s = _; d } ->
        let before = String.Set.length tracker in
        print_endline (Yojson.Safe.to_string d);
        let tracker, member = begin match Events.Voice_state_update.of_yojson_exn d with
        (* Target channel, not a bot *)
        | { channel_id = Some channel_id; user_id; member; _ }
          when (Channel.is_bot member |> not) && String.(=) channel_id config.vc_channel ->
          String.Set.add tracker user_id, member
        (* Anything else *)
        | { user_id; member; _ } -> String.Set.remove tracker user_id, member
        end
        in
        let after = String.Set.length tracker in
        let%lwt () = vc_member_change config ~before ~after (VSU member) in
        Lwt.return { tracker }

      (* GUILD_CREATE *)
      | { op = Dispatch; t = Some "GUILD_CREATE"; s = _; d } ->
        let before = String.Set.length tracker in
        let gc = Events.Guild_create.of_yojson_exn d in
        let users_in_target_channel = begin match gc with
        | { voice_states = None; _ } -> String.Set.empty
        | { voice_states = Some ll; _ } ->
          List.fold ll ~init:String.Set.empty ~f:(fun acc -> function
          | { channel_id = Some channel_id; user_id; _ } when String.(=) channel_id config.vc_channel ->
            String.Set.add acc user_id
          | _ -> acc
          )
        end
        in
        let tracker, members = begin match gc.members with
        | None -> String.Set.empty, []
        | Some members ->
          List.fold members ~init:(String.Set.empty, []) ~f:(fun ((set, ll) as acc) -> function
          | (Channel.{ user = Some { id; _ }; _ } as member) when String.Set.mem users_in_target_channel id ->
            if String.Set.mem set id
            then acc
            else (String.Set.add set id, (member :: ll))
          | _ -> acc
          )
        end
        in
        let after = String.Set.length tracker in
        let%lwt () = vc_member_change config ~before ~after (GC members) in
        Lwt.return { tracker }

      (* Other events *)
      | _ -> Lwt.return state
    end)
  in
  Bot.start
