open! Core_kernel

module Opcode : sig
  type t =
  | Dispatch
  | Heartbeat
  | Identify
  | Presence_update
  | Voice_state_update
  | Resume
  | Reconnect
  | Request_guild_members
  | Invalid_session
  | Hello
  | Heartbeat_ACK
  [@@deriving sexp, yojson]

  val to_name : t -> string
end

module Recv : sig
  type t = {
    op: Opcode.t;
    t: string option;
    s: int option;
    d: Yojson.Safe.t;
  }
  [@@deriving sexp_of, fields, yojson { exn = true }]
end

module Send : sig
  type t = {
    op: Opcode.t;
    t: string option [@default None];
    s: int option [@default None];
    d: Yojson.Safe.t;
  }
  [@@deriving sexp_of, fields, to_yojson]
end

module Private : sig
  val of_string : int option ref -> string -> Recv.t
end
