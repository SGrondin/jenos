open! Core_kernel

module Hello : sig
  type t = {
    heartbeat_interval: int;
  }
  [@@deriving sexp, fields, yojson] [@@unboxed]
end

module Invalid_session : sig
  type t = {
    must_reconnect: bool;
  }
  [@@deriving sexp, fields, yojson] [@@unboxed]
end

module Ready : sig
  type t = {
    v: int;
    user: Objects.User.t;
    private_channels: unit list;
    session_id: string;
    shard: int list option;
  }
  [@@deriving sexp, fields, yojson]
end

module Voice_state_update : sig
  type t = {
    guild_id: string option;
    channel_id: string option;
    user_id: string;
    member: Objects.Channel.member option;
    session_id: string;
  }
  [@@deriving sexp, fields, yojson]
end

module Guild_create : sig
  type voice_state = {
    user_id: string;
    session_id: string;
    channel_id: string option;
  }
  [@@deriving sexp, fields, yojson]

  type t = {
    id: string;
    name: string;
    member_count: int option;
    members: Objects.Channel.member list option;
    voice_states: voice_state list option;
  }
  [@@deriving sexp, fields, yojson]
end
