open! Core_kernel

open Discord

let random_state = Random.State.make_self_init ()
let () = Random.set_state random_state

module Regex : sig
  type t [@@deriving sexp_of, of_yojson]

  val matching: t -> string -> bool
end = struct
  type t = Re.re * string

  let sexp_of_t (_re, s) = Sexp.Atom s
  let of_yojson = function
  | `String s ->
    Ok (Re.Perl.re ~opts:[`Caseless] s |> Re.Perl.compile, s)
  | json -> Error (sprintf "Impossible to parse JSON %s into a regex type" (Yojson.Safe.to_string json))

  let matching (re, _s) str = Re.execp re str
end

type reaction = {
  regex: Regex.t;
  emojis: string list;
} [@@deriving sexp_of, of_yojson { exn = true }]

type thresholds = {
  p_common: int;
  p_uncommon: int;
} [@@deriving sexp_of, of_yojson { exn = true }]

type lines = {
  common: string array;
  uncommon: string array;
  rare: string array;
} [@@deriving sexp_of, of_yojson { exn = true }]

type config = {
  token: string;
  status: Commands.Identify.activity option [@default None];
  reactions: reaction list;
  thresholds: thresholds;
  line2: lines;
  line4: lines;
  vc_channel: string;
  text_channel: string;
} [@@deriving sexp_of, of_yojson { exn = true }]

type vc_change =
(* Voice State Update *)
| VSU of Objects.Channel.member option
(* Guild Create *)
| GC of Objects.Channel.member list

let latch_all = Latch.(create ~cooldown:Int64.(20L * Time.min))
let latch_notif = Latch.(create ~cooldown:Int64.(30L * Time.min))

let can_send ~notifies =
  let now = Latch.Time.get () in
  let b =
    if notifies
    then Latch.check ~now latch_all && Latch.check ~now latch_notif
    else Latch.check ~now latch_all
  in
  if b then begin
    Latch.trigger ~now latch_all;
    if notifies then Latch.trigger ~now latch_notif
  end;
  b

let post_message ~token ~channel_id ~content ~notifies =
  if can_send ~notifies
  then Rest.Channel.create_message ~token ~channel_id ~content
  else Lwt_io.printl "⏳ Waiting until we can send again"

let pick_line { p_common; p_uncommon } lines =
  begin match Random.int_incl 1 100 with
  | r when r <= p_common -> lines.common
  | r when r <= p_uncommon -> lines.uncommon
  | _ -> lines.rare
  end
  |> Array.random_element_exn

let vc_member_change { token; text_channel = channel_id; line2; line4; thresholds; _ } ~before ~after change =
  let nick_opt opt =
    Option.bind opt ~f:Objects.Channel.member_name
    |> Option.value ~default:"❓"
  in
  let nick member =
    Objects.Channel.member_name member
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
    let content = pick_line thresholds line2 in
    let%lwt () = post_message ~token ~channel_id ~content ~notifies:true in
    Lwt_io.printlf "✅ Posted to <%s>" channel_id
  | 4 ->
    let content = pick_line thresholds line4 in
    let%lwt () = post_message ~token ~channel_id ~content ~notifies:false in
    Lwt_io.printlf "✅ Posted to <%s>" channel_id
  | _ -> Lwt.return_unit
  end
  else Lwt.return_unit

let create_bot config =
  let module Bot = Bot.Make (struct
      type t = {
        tracker: String.Set.t;
      } [@@deriving sexp]

      let initial () = { tracker = String.Set.empty }

      let (>>>) f x = Lwt.map (fun () -> x) f

      let before_reidentifying state = Lwt_io.printl "⏯️ Resuming..." >>> state
      let before_reconnecting state = Lwt_io.printl "🌐 Reconnecting..." >>> state
      let on_connection_closed state = Lwt_io.eprintl "🔌 Connection was closed." >>> state
      let on_exn exn = Lwt_io.eprintlf "❌ Unexpected error: %s" (Exn.to_string exn)

      let before_action ~respond:_ state : Message.Recv.t -> t Lwt.t = function
      (* READY *)
      | { op = Dispatch; t = Some "READY"; s = _; d } ->
        let%lwt () = Lwt_io.printlf "✅ READY! %s" (Yojson.Safe.to_string d) in
        Lwt.return state

      (* RECONNECT *)
      | { op = Reconnect; _ } ->
        let%lwt () = Lwt_io.printl "⚠️ Received a Reconnect request." in
        Lwt.return state

      (* RESUMED *)
      | { op = Dispatch; t = Some "RESUMED"; s = _; d = _ } ->
        let%lwt () = Lwt_io.printl "▶️ Resumed" in
        Lwt.return state

      (* INVALID_SESSION *)
      | { op = Invalid_session; _ } ->
        let%lwt () = Lwt_io.eprintl "⚠️ Session rejected, starting a new session..." in
        Lwt.return state

      | _ -> Lwt.return state

      let after_action ~respond:_ ({ tracker } as state) : Message.Recv.t -> t Lwt.t = function
      (* VOICE_STATE_UPDATE *)
      | { op = Dispatch; t = Some "VOICE_STATE_UPDATE"; s = _; d } ->
        let before = String.Set.length tracker in
        let tracker, member = begin match Events.Voice_state_update.of_yojson_exn d with
        (* Target channel, not a bot *)
        | { channel_id = Some channel_id; user_id; member; _ }
          when (Objects.Channel.is_bot member |> not) && String.(=) channel_id config.vc_channel ->
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
          | (Objects.Channel.{ user = Some { id; _ }; _ } as member) when String.Set.mem users_in_target_channel id ->
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

      (* MESSAGE_CREATE *)
      | { op = Dispatch; t = Some "MESSAGE_CREATE"; s = _; d } ->
        begin match Objects.Message.of_yojson_exn d with
        | { id = message_id; type_ = DEFAULT; channel_id; content; _ } ->
          let emojis =
            List.filter_map config.reactions ~f:(function
            | { regex; emojis } when Regex.matching regex content -> Some emojis
            | _ -> None
            )
            |> List.concat_no_order
          in
          Rest.Call.background ~on_exn (fun () ->
            Lwt_list.iter_s (fun emoji ->
              Rest.Channel.create_reaction ~token:config.token ~channel_id ~message_id ~emoji
            ) emojis
          );
          Lwt.return state
        | _ -> Lwt.return state
        end

      (* Other events *)
      | _ -> Lwt.return state
    end)
  in
  Bot.start
