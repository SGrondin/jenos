open! Core_kernel

open Protocol.Recv

type parsed =
| Hello of Events.Hello.t
| Invalid_session of Events.Invalid_session.t
| Reconnect
| Ready of Events.Ready.t
| Resumed
| Voice_state_update of Events.Voice_state_update.t
| Guild_create of Events.Guild_create.t
| Message_create of Objects.Message.t
| Other
[@@deriving sexp_of]

type t = {
  raw: Protocol.Recv.t;
  parsed: parsed;
} [@@deriving sexp_of]

let of_recv = function
| { op = Hello; d; _ } -> Hello (Events.Hello.of_yojson_exn d)
| { op = Invalid_session; d; _ } -> Invalid_session (Events.Invalid_session.of_yojson_exn d)
| { op = Reconnect; _ } -> Reconnect
| { op = Dispatch; t = Some "READY"; s = _; d } -> Ready (Events.Ready.of_yojson_exn d)
| { op = Dispatch; t = Some "RESUMED"; s = _; d = _ } -> Resumed
| { op = Dispatch; t = Some "VOICE_STATE_UPDATE"; s = _; d } -> Voice_state_update (Events.Voice_state_update.of_yojson_exn d)
| { op = Dispatch; t = Some "GUILD_CREATE"; s = _; d } -> Guild_create (Events.Guild_create.of_yojson_exn d)
| { op = Dispatch; t = Some "MESSAGE_CREATE"; s = _; d } -> Message_create (Objects.Message.of_yojson_exn d)
| _ -> Other

let parse raw =
  { raw; parsed = of_recv raw }
