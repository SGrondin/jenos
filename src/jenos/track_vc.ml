open! Core_kernel
open Discord
open Config

type state = {
  just_started: bool;
  tracker: String.Set.t;
}

let initial_state = { just_started = true; tracker = String.Set.empty }

type vc_change =
  (* Voice State Update *)
  | VSU of Objects.Channel.member option
  (* Guild Create *)
  | GC  of Objects.Channel.member list

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
    let%lwt () = Rest.Channel.create_message ~token ~channel_id ~content Ignore in
    Lwt_io.printlf "✅ Posted to <%s>" channel_id
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
  let nick_opt opt = Option.bind opt ~f:Objects.Channel.member_name |> Option.value ~default:"❓" in
  let nick member = Objects.Channel.member_name member |> Option.value ~default:"❓" in
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

let on_voice_state_update config { tracker; just_started } (vsu : Events.Voice_state_update.t) =
  let before = String.Set.length tracker in
  let tracker, member =
    match vsu with
    (* Target channel, not a bot *)
    | { channel_id = Some channel_id; user_id; member; _ }
      when Objects.Channel.is_bot member |> not && String.( = ) channel_id config.vc_channel ->
      String.Set.add tracker user_id, member
    (* Anything else *)
    | { user_id; member; _ } -> String.Set.remove tracker user_id, member
  in
  let after = String.Set.length tracker in
  let%lwt () = vc_member_change config ~just_started ~before ~after (VSU member) in
  Lwt.return tracker

let on_guild_create config { tracker; just_started } (gc : Events.Guild_create.t) =
  let before = String.Set.length tracker in
  let users_in_target_channel =
    match gc with
    | { voice_states = None; _ } -> String.Set.empty
    | { voice_states = Some ll; _ } ->
      List.fold ll ~init:String.Set.empty ~f:(fun acc -> function
        | { channel_id = Some channel_id; user_id; _ } when String.( = ) channel_id config.vc_channel ->
          String.Set.add acc user_id
        | _ -> acc)
  in
  let tracker, members =
    match gc.members with
    | None -> String.Set.empty, []
    | Some members ->
      List.fold members ~init:(String.Set.empty, []) ~f:(fun ((set, ll) as acc) -> function
        | Objects.Channel.{ user = Some { id; _ }; _ } as member
          when String.Set.mem users_in_target_channel id ->
          if String.Set.mem set id then acc else String.Set.add set id, member :: ll
        | _ -> acc)
  in
  let after = String.Set.length tracker in
  let%lwt () = vc_member_change config ~just_started ~before ~after (GC members) in
  Lwt.return tracker
