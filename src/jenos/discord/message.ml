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
[@@deriving sexp, variants]

type t = {
  raw: Protocol.Recv.t;
  parsed: parsed;
} [@@deriving sexp]

let load d p f = d |> p |> Result.ok_or_failwith |> f

let of_recv = function
| { op = Hello; d; _ } -> load d Events.Hello.of_yojson hello
| { op = Invalid_session; d; _ } -> load d Events.Invalid_session.of_yojson invalid_session
| { op = Reconnect; _ } -> Reconnect
| { op = Dispatch; t = Some "READY"; s = _; d } -> load d Events.Ready.of_yojson ready
| { op = Dispatch; t = Some "RESUMED"; s = _; d = _ } -> Resumed
| { op = Dispatch; t = Some "VOICE_STATE_UPDATE"; s = _; d } -> load d Events.Voice_state_update.of_yojson voice_state_update
| { op = Dispatch; t = Some "GUILD_CREATE"; s = _; d } -> load d Events.Guild_create.of_yojson guild_create
| { op = Dispatch; t = Some "MESSAGE_CREATE"; s = _; d } -> load d Objects.Message.of_yojson message_create
| _ -> Other

let parse raw =
  { raw; parsed = of_recv raw }
