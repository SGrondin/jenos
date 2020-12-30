open! Core_kernel
open Discord
open Config
module SnowflakeSet = Set.Make (Basics.Snowflake)

type state = {
  just_started: bool;
  tracker: SnowflakeSet.t;
}

let initial_state = { just_started = true; tracker = SnowflakeSet.empty }

type vc_change =
  (* Voice State Update *)
  | VSU of Data.User.member option
  (* Guild Create *)
  | GC  of Data.User.member list

let latch_all = Latch.(create ~cooldown:(Time.min 20L))

let latch_notif = Latch.(create ~cooldown:(Time.min 40L))

let can_send ~notifies =
  let now = Latch.Time.get () in
  let b =
    if notifies
    then Latch.check ~now latch_all && Latch.check ~now latch_notif
    else Latch.check ~now latch_all
  in
  if b
  then begin
    Latch.trigger ~now latch_all;
    if notifies then Latch.trigger ~now latch_notif
  end;
  b

let post_message ~token ~channel_id ~content ~notifies =
  if can_send ~notifies
  then begin
    let%lwt _message = Rest.Channel.create_message ~token ~channel_id ~content in
    Lwt_io.printlf "✅ Posted to <%s>" (Basics.Snowflake.to_string channel_id)
  end
  else Lwt_io.printl "⏳ Waiting until we can send again"

let pick_line { p_common; p_uncommon } lines =
  begin
    match Random.int_incl 1 100 with
    | r when r <= p_common -> lines.common
    | r when r <= p_uncommon -> lines.uncommon
    | _ -> lines.rare
  end
  |> Array.random_element_exn

let vc_member_change { token; text_channel = channel_id; line2; line4; thresholds; _ } ~just_started
   ~before ~after change =
  let nick_opt opt = Option.bind opt ~f:Data.User.Util.member_name |> Option.value ~default:"❓" in
  let nick member = Data.User.Util.member_name member |> Option.value ~default:"❓" in
  let buf = Buffer.create 64 in
  begin
    match change with
    | VSU member when before < after -> bprintf buf "📈 %s " (nick_opt member)
    | VSU member when before > after -> bprintf buf "📉 %s " (nick_opt member)
    | VSU _
     |GC [] ->
      ()
    | GC ll ->
      List.map ll ~f:(fun m -> sprintf "⚡ %s" (nick m)) |> String.concat ~sep:"\n" |> bprintf buf "%s\n"
  end;
  if after <> before then bprintf buf "Current count: %d" after;
  let%lwt () = if Buffer.length buf > 0 then Lwt_io.printl (Buffer.contents buf) else Lwt.return_unit in
  if after > before && not just_started
  then begin
    match after with
    | 2 ->
      let content = pick_line thresholds line2 in
      post_message ~token ~channel_id ~content ~notifies:true
    | 4 ->
      let content = pick_line thresholds line4 in
      post_message ~token ~channel_id ~content ~notifies:false
    | _ -> Lwt.return_unit
  end
  else Lwt.return_unit

let on_voice_state_update config { tracker; just_started } (vsu : Data.Voice_state.t) =
  let before = SnowflakeSet.length tracker in
  let tracker, member =
    match vsu with
    (* Target channel, not a bot *)
    | { channel_id = Some channel_id; user_id; member; _ }
      when Data.User.Util.is_bot member |> not && Basics.Snowflake.equal channel_id config.vc_channel ->
      SnowflakeSet.add tracker user_id, member
    (* Anything else *)
    | { user_id; member; _ } -> SnowflakeSet.remove tracker user_id, member
  in
  let after = SnowflakeSet.length tracker in
  let%lwt () = vc_member_change config ~just_started ~before ~after (VSU member) in
  Lwt.return tracker

let on_guild_create config { tracker; just_started } (gc : Data.Guild.t) =
  let before = SnowflakeSet.length tracker in
  let users_in_target_channel =
    match gc with
    | { voice_states = None; _ } -> SnowflakeSet.empty
    | { voice_states = Some ll; _ } ->
      List.fold ll ~init:SnowflakeSet.empty ~f:(fun acc -> function
        | { channel_id = Some channel_id; user_id; _ }
          when Basics.Snowflake.equal channel_id config.vc_channel ->
          SnowflakeSet.add acc user_id
        | _ -> acc)
  in
  let tracker, members =
    match gc.members with
    | None -> SnowflakeSet.empty, []
    | Some members ->
      List.fold members ~init:(SnowflakeSet.empty, []) ~f:(fun ((set, ll) as acc) -> function
        | { user = Some { id; _ }; _ } as member when SnowflakeSet.mem users_in_target_channel id ->
          if SnowflakeSet.mem set id then acc else SnowflakeSet.add set id, member :: ll
        | _ -> acc)
  in
  let after = SnowflakeSet.length tracker in
  let%lwt () = vc_member_change config ~just_started ~before ~after (GC members) in
  Lwt.return tracker
