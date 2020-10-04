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
  [@@deriving sexp]

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
  | x -> Error (sprintf "Invalid opcode type: %s" (Yojson.Safe.to_string x))
end

module Yojson = struct
  include Yojson
  module Safe = struct
    include Safe
    let sexp_of_t (json : Yojson.Safe.t) : Sexp.t =
      let rec loop = function
      | `String s -> Sexp.Atom s
      | `Int x -> Sexp.Atom (Int.to_string x)
      | `Float x -> Sexp.Atom (Float.to_string x)
      | `Bool x -> Sexp.Atom (Bool.to_string x)
      | `Null -> Sexp.List []
      | `List ll -> Sexp.List (List.map ll ~f:loop)
      | `Assoc ll -> Sexp.List (List.map ll ~f:(fun (k, v) ->
          Sexp.List [Sexp.Atom k; loop v]
        ))
      in
      Yojson.Safe.to_basic json |> loop
  end
end


module Recv = struct
  type t = {
    op: Opcode.t;
    t: string option;
    s: int option;
    d: Yojson.Safe.t;
  }
  [@@deriving sexp_of, yojson { exn = true }]
end

module Send = struct
  let (=) = Poly.(=)
  type t = {
    op: Opcode.t;
    t: string option [@default None];
    s: int option [@default None];
    d: Yojson.Safe.t;
  }
  [@@deriving sexp_of, yojson]
  let (=) = Core_kernel.(=)
end

let heartbeat_seq : int option ref = ref None

let of_string raw =
  let message = Yojson.Safe.from_string raw |> Recv.of_yojson_exn in
  Option.iter message.s ~f:(fun seq -> heartbeat_seq := Some seq);
  message

let respond send message =
  let open Websocket in
  let content = Send.to_yojson message |> Yojson.Safe.to_string in
  let%lwt () = Lwt_io.printlf ">>> %s" content in
  Frame.create ~opcode:Frame.Opcode.Text ~content ()
  |> send
