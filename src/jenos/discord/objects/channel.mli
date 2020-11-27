open! Core_kernel

type type_ =
| GUILD_TEXT
| DM
| GUILD_VOICE
| GROUP_DM
| GUILD_CATEGORY
| GUILD_NEWS
| GUILD_STORE
[@@deriving sexp, yojson]

type t = {
  id: string;
  type_: type_;
  position: int option;
  nsfw: bool option;
  name: string option;
  user_limit: int option;
  bitrate: int option;
}
[@@deriving sexp, fields, yojson]

type member = {
  user: User.t option;
  nick: string option;
  roles: string list;
}
[@@deriving sexp, fields, yojson]

val is_bot : member option -> bool

val member_id : member -> string option

val member_name : member -> string option
