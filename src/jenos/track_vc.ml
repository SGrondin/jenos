open! Core_kernel

open Discord
open Config

type vc_change =
(* Voice State Update *)
| VSU of Objects.Channel.member option
(* Guild Create *)
| GC of Objects.Channel.member list

let latch_all = Latch.(create ~cooldown:Int64.(20L * Time.min))
let latch_notif = Latch.(create ~cooldown:Int64.(40L * Time.min))

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
  then Rest.Channel.create_message ~token ~channel_id ~content Ignore
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

let on_voice_state_update config tracker (vsu : Events.Voice_state_update.t) =
  let before = String.Set.length tracker in
  let tracker, member = begin match vsu with
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
  Lwt.return tracker

let on_guild_create config tracker (gc : Events.Guild_create.t) =
  let before = String.Set.length tracker in
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
  Lwt.return tracker
