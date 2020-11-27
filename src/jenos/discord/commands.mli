open! Core_kernel

module Identify : sig
  type connection = {
    os: string [@key "$os"];
    browser: string [@key "$browser"];
    device: string [@key "$device"];
  }
  [@@deriving sexp, fields, yojson { strict = false }]

  type activity =
  | Game of string
  | Streaming of string
  | Listening of string
  | Competing of string
  [@@deriving sexp, yojson { strict = false }]

  type status =
  | Online
  | DND
  | Idle
  | Invisible
  | Offline
  [@@deriving sexp, yojson { strict = false }]

  type presence = {
    since: Int64.t option;
    activities: activity list option;
    status: status;
    afk: bool;
  }
  [@@deriving sexp, fields, yojson { strict = false }]
  type t = {
    token: string;
    properties: connection;
    compress: bool;
    presence: presence;
    guild_subscriptions: bool;
    intents: int;
  }
  [@@deriving sexp, fields, yojson { strict = false }]

  val to_message : t -> Protocol.Send.t
end

module Heartbeat : sig
  type t = int option [@@deriving sexp, yojson { strict = false }]

  val to_message : t -> Protocol.Send.t
end

module Heartbeat_ACK : sig
  val to_message : Protocol.Send.t
end

module Resume : sig
  type t = {
    token: string;
    session_id: string;
    seq: int option;
  }
  [@@deriving sexp, fields, yojson { strict = false }]

  val to_message : t -> Protocol.Send.t
end
