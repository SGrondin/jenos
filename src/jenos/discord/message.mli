open! Core_kernel

type parsed =
  | Hello              of Events.Hello.t
  | Invalid_session    of Events.Invalid_session.t
  | Reconnect
  | Ready              of Events.Ready.t
  | Resumed
  | Voice_state_update of Events.Voice_state_update.t
  | Guild_create       of Events.Guild_create.t
  | Message_create     of Objects.Message.t
  | Other
[@@deriving sexp]

type t = {
  raw: Protocol.Recv.t;
  parsed: parsed;
}
[@@deriving sexp]

val parse : Protocol.Recv.t -> t
