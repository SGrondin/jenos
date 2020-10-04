open! Core_kernel

let t0 =
  let open Int63 in
  Time_now.nanoseconds_since_unix_epoch () / (of_int 1_000_000)
  |> to_int64

module Identify = struct
  type connection = {
    os: string [@key "$os"];
    browser: string [@key "$browser"];
    device: string [@key "$device"];
  }
  [@@deriving sexp, yojson]
  type activity = {
    name: string;
    type_: int [@key "type"];
    created_at: Int64.t;
  }
  [@@deriving sexp, yojson]
  type presence = {
    since: Int64.t option;
    activities: activity list option;
    status: string;
    afk: bool;
  }
  [@@deriving sexp, yojson]
  type t = {
    token: string;
    properties: connection;
    compress: bool;
    presence: presence;
    guild_subscriptions: bool;
    intents: int;
  }
  [@@deriving sexp, yojson]
  let to_message x =
    let d = to_yojson x in
    Message.Send.{ op = Message.Opcode.Identify; t = None; s = None; d }
end

module Heartbeat = struct
  type t = int option
  let to_message : t -> Message.Send.t = function
  | None -> { op = Message.Opcode.Heartbeat; t = None; s = None; d = `Null }
  | Some x -> { op = Message.Opcode.Heartbeat; t = None; s = None; d = `Int x }
end

module Heartbeat_ACK = struct
  type t = unit
  let to_message () =
    Message.Send.{ op = Message.Opcode.Heartbeat_ACK; t = None; s = None; d = `Null }
end
