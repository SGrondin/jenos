open! Core_kernel

module Opcode = struct
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
    | Unknown               of int
  [@@deriving sexp, variants]

  let to_name = Variants.to_name

  let to_yojson : t -> Yojson.Safe.t = function
  | Dispatch -> `Int 0
  | Heartbeat -> `Int 1
  | Identify -> `Int 2
  | Presence_update -> `Int 3
  | Voice_state_update -> `Int 4
  | Resume -> `Int 6
  | Reconnect -> `Int 7
  | Request_guild_members -> `Int 8
  | Invalid_session -> `Int 9
  | Hello -> `Int 10
  | Heartbeat_ACK -> `Int 11
  | Unknown x -> `Int x

  let of_yojson : Yojson.Safe.t -> (t, string) result = function
  | `Int 0 -> Ok Dispatch
  | `Int 1 -> Ok Heartbeat
  | `Int 2 -> Ok Identify
  | `Int 3 -> Ok Presence_update
  | `Int 4 -> Ok Voice_state_update
  | `Int 6 -> Ok Resume
  | `Int 7 -> Ok Reconnect
  | `Int 8 -> Ok Request_guild_members
  | `Int 9 -> Ok Invalid_session
  | `Int 10 -> Ok Hello
  | `Int 11 -> Ok Heartbeat_ACK
  | `Int x -> Ok (Unknown x)
  | x -> Error (sprintf "Invalid opcode type: %s" (Yojson.Safe.to_string x))
end

module Yojson = struct
  include Yojson

  module Safe = struct
    include Safe

    let sexp_of_t json = Sexp.Atom (Yojson.Safe.to_string json)

    let t_of_sexp = function
    | Sexp.Atom s -> Yojson.Safe.from_string s
    | sexp -> failwithf "Invalid S-Exp for JSON: %s" (Sexp.to_string sexp) ()
  end
end

module Recv = struct
  type t = {
    op: Opcode.t;
    t: string option; [@default None]
    s: int option; [@default None]
    d: Yojson.Safe.t; [@default `Assoc []]
  }
  [@@deriving sexp, fields, yojson]
end

module Send = struct
  type t = {
    op: Opcode.t;
    t: string option; [@default None]
    s: int option; [@default None]
    d: Yojson.Safe.t;
  }
  [@@deriving sexp, fields, yojson]
end
