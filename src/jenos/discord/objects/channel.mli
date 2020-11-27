open! Core_kernel

type type_ =
| GUILD_TEXT
| DM
| GUILD_VOICE
| GROUP_DM
| GUILD_CATEGORY
| GUILD_NEWS
| GUILD_STORE
[@@deriving sexp, yojson { strict = false }]

type t = {
  id: string;
  type_: type_ [@key "type"];
  position: int option [@default None];
  nsfw: bool option [@default None];
  name: string option [@default None];
  user_limit: int option [@default None];
  bitrate: int option [@default None];
}
[@@deriving sexp, fields, yojson { strict = false }]

type member = {
  user: User.t option [@default None];
  nick: string option [@default None];
  roles: string list;
}
[@@deriving sexp, fields, yojson { strict = false }]

val is_bot : member option -> bool

val member_id : member -> string option

val member_name : member -> string option
