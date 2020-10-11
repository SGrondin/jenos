open! Core_kernel

let t0 =
  let open Int63 in
  Time_now.nanoseconds_since_unix_epoch () / (of_int 1_000_000)
  (* |> (fun x -> x - (of_int64_exn 1_420_070_400_000L)) *)
  |> to_int64

module Identify = struct
  type connection = {
    os: string [@key "$os"];
    browser: string [@key "$browser"];
    device: string [@key "$device"];
  }
  [@@deriving sexp, fields, to_yojson]
  type activity_type =
  | Game of string
  | Streaming of string
  | Listening of string
  | Custom of { emoji: string; name: string }
  | Competing of string
  [@@deriving sexp]
  type activity = {
    type_: activity_type [@key "type"];
    created_at: Int64.t;
  }
  [@@deriving sexp, fields]
  let activity_to_yojson activity : Yojson.Safe.t =
    let name, type_ = begin match activity.type_ with
    | Game s -> (sprintf "Playing %s" s), 0
    | Streaming s -> (sprintf "Streaming %s" s), 1
    | Listening s -> (sprintf "Listening to %s" s), 2
    | Custom { emoji; name } -> (sprintf ":%s: %s" emoji name), 4
    | Competing s -> (sprintf "Competing in %s" s), 5
    end
    in
    `Assoc [
      "name", `String name;
      "type", `Int type_;
      "created_at", `Intlit (Int64.to_string activity.created_at);
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
