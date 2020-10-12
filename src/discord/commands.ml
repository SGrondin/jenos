open! Core_kernel

module Identify = struct
  type connection = {
    os: string [@key "$os"];
    browser: string [@key "$browser"];
    device: string [@key "$device"];
  }
  [@@deriving sexp, fields, to_yojson]
  type activity =
  | Game of string
  | Streaming of string
  | Listening of string
  | Competing of string
  [@@deriving sexp]
  let activity_to_yojson activity : Yojson.Safe.t =
    let name, type_ = begin match activity with
    | Game s -> s, 0
    | Streaming s -> s, 1
    | Listening s -> s, 2
    | Competing s -> s, 5
    end
    in
    `Assoc [
      "name", `String name;
      "type", `Int type_;
    ]

  type presence = {
    since: Int64.t option;
    activities: activity list option;
    status: string;
    afk: bool;
  }
  [@@deriving sexp, fields, to_yojson]
  type t = {
    token: string;
    properties: connection;
    compress: bool;
    presence: presence;
    guild_subscriptions: bool;
    intents: int;
  }
  [@@deriving sexp, fields, to_yojson]
  let to_message x : Message.Send.t = {
    op = Message.Opcode.Identify;
    t = None;
    s = None;
    d = to_yojson x;
  }
end

module Heartbeat = struct
  type t = int option
  let to_message x : Message.Send.t = {
    op = Message.Opcode.Heartbeat;
    t = None;
    s = None;
    d = Option.value_map x ~default:`Null ~f:(fun x -> `Int x);
  }
end

module Heartbeat_ACK = struct
  let to_message : Message.Send.t = {
    op = Message.Opcode.Heartbeat_ACK;
    t = None;
    s = None;
    d = `Null;
  }
end

module Resume = struct
  type t = {
    session_id: string;
    seq: int option;
  }
  [@@deriving sexp, fields, to_yojson]
  let to_message x : Message.Send.t = {
    op = Message.Opcode.Resume;
    t = None;
    s = None;
    d = to_yojson x;
  }
end
