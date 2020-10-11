open! Core_kernel

module Hello = struct
  type t = {
    heartbeat_interval: int;
  }
  [@@deriving sexp, fields, of_yojson { exn = true; strict = false }] [@@unboxed]
end

module Ready = struct
  type t = {
    v: int;
    user: User.t;
    private_channels: unit list;
    session_id: string;
    shard: int list option [@default None];
  }
  [@@deriving sexp, fields, of_yojson { exn = true; strict = false }]
end

module Voice_state_update = struct
  type member = {
    user: User.t option [@default None];
    nick: string option;
    roles: string list;
  }
  [@@deriving sexp, fields, of_yojson { exn = true; strict = false }]
  type t = {
    guild_id: string option [@default None];
    channel_id: string option;
    user_id: string;
    member: member option [@default None];
    session_id: string;
  }
  [@@deriving sexp, fields, of_yojson { exn = true; strict = false }]
end
