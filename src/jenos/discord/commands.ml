open! Core_kernel

module Identify = struct
  type connection = {
    os: string; [@key "$os"]
    browser: string; [@key "$browser"]
    device: string; [@key "$device"]
  }
  [@@deriving sexp, fields, yojson { strict = false }]

  type activity =
    | Game      of string
    | Streaming of string
    | Listening of string
    | Competing of string
  [@@deriving sexp]

  let activity_to_yojson activity : Yojson.Safe.t =
    let name, type_ =
      match activity with
      | Game s -> s, 0
      | Streaming s -> s, 1
      | Listening s -> s, 2
      | Competing s -> s, 5
    in
    `Assoc [ "name", `String name; "type", `Int type_ ]

  let activity_of_yojson = function
  | `Assoc [ ("game", `String s) ] -> Ok (Game s)
  | `Assoc [ ("streaming", `String s) ] -> Ok (Streaming s)
  | `Assoc [ ("listening", `String s) ] -> Ok (Listening s)
  | `Assoc [ ("competing", `String s) ] -> Ok (Competing s)
  | json -> Error (sprintf "Invalid activity: %s" (Yojson.Safe.to_string json))

  type status =
    | Online
    | DND
    | Idle
    | Invisible
    | Offline
  [@@deriving sexp]

  let status_to_yojson = function
  | Online -> `String "online"
  | DND -> `String "dnd"
  | Idle -> `String "idle"
  | Invisible -> `String "invisible"
  | Offline -> `String "offline"

  let status_of_yojson = function
  | `String "online" -> Ok Online
  | `String "dnd" -> Ok DND
  | `String "idle" -> Ok Idle
  | `String "invisible" -> Ok Invisible
  | `String "offline" -> Ok Offline
  | json -> Error (sprintf "Invalid status: %s" (Yojson.Safe.to_string json))

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

  let to_message x : Protocol.Send.t =
    { op = Protocol.Opcode.Identify; t = None; s = None; d = to_yojson x }
end

module Heartbeat = struct
  type t = int option [@@deriving sexp, yojson { strict = false }]

  let to_message x : Protocol.Send.t =
    {
      op = Protocol.Opcode.Heartbeat;
      t = None;
      s = None;
      d = Option.value_map x ~default:`Null ~f:(fun x -> `Int x);
    }
end

module Heartbeat_ACK = struct
  let to_message : Protocol.Send.t = { op = Protocol.Opcode.Heartbeat_ACK; t = None; s = None; d = `Null }
end

module Resume = struct
  type t = {
    token: string;
    session_id: string;
    seq: int option;
  }
  [@@deriving sexp, fields, yojson { strict = false }]

  let to_message x : Protocol.Send.t =
    { op = Protocol.Opcode.Resume; t = None; s = None; d = to_yojson x }
end
