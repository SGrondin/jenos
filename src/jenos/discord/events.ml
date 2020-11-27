open! Core_kernel

module Hello = struct
  type t = {
    heartbeat_interval: int;
  }
  [@@deriving sexp, fields, yojson { strict = false }] [@@unboxed]
end

module Invalid_session = struct
  type t = {
    must_reconnect: bool;
  }
  [@@deriving sexp, fields] [@@unboxed]
  let of_yojson x =
    [%of_yojson: bool] x
    |> Result.map ~f:(fun must_reconnect -> { must_reconnect })

  let to_yojson { must_reconnect } = `Bool must_reconnect
end

module Ready = struct
  let (=) = Poly.(=)
  type t = {
    v: int;
    user: Objects.User.t;
    private_channels: unit list;
    session_id: string;
    shard: int list option [@default None];
  }
  [@@deriving sexp, fields, yojson { strict = false }]
end

module Voice_state_update = struct
  let (=) = Poly.(=)
  type t = {
    guild_id: string option [@default None];
    channel_id: string option;
    user_id: string;
    member: Objects.Channel.member option [@default None];
    session_id: string;
  }
  [@@deriving sexp, fields, yojson { strict = false }]
end

module Guild_create = struct
  type voice_state = {
    user_id: string;
    session_id: string;
    channel_id: string option;
  }
  [@@deriving sexp, fields, yojson { strict = false }]

  let (=) = Poly.(=)
  type t = {
    id: string;
    name: string;
    member_count: int option [@default None];
    members: Objects.Channel.member list option [@default None];
    voice_states: voice_state list option [@default None];
  }
  [@@deriving sexp, fields, yojson { strict = false }]
end
