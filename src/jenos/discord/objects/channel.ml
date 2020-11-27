open! Core_kernel

type type_ =
| GUILD_TEXT
| DM
| GUILD_VOICE
| GROUP_DM
| GUILD_CATEGORY
| GUILD_NEWS
| GUILD_STORE
[@@deriving sexp]
let type__of_yojson = function
| `Int 0 -> Ok GUILD_TEXT
| `Int 1 -> Ok DM
| `Int 2 -> Ok GUILD_VOICE
| `Int 3 -> Ok GROUP_DM
| `Int 4 -> Ok GUILD_CATEGORY
| `Int 5 -> Ok GUILD_NEWS
| `Int 6 -> Ok GUILD_STORE
| json -> Error (sprintf "Impossible to parse JSON %s into a channel type" (Yojson.Safe.to_string json))

let type__to_yojson = function
| GUILD_TEXT -> `Int 0
| DM -> `Int 1
| GUILD_VOICE -> `Int 2
| GROUP_DM -> `Int 3
| GUILD_CATEGORY -> `Int 4
| GUILD_NEWS -> `Int 5
| GUILD_STORE -> `Int 6

let (=) = Poly.(=)
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

let is_bot member_opt =
  Option.Monad_infix.(member_opt >>= user >>= User.bot)
  |> Option.value ~default:false

let member_id = function
| { user = Some { id; _ }; _ } -> Some id
| _ -> None

let member_name = function
| { nick = Some username; _ }
| { user = Some { username; _ }; _ } -> Some username
| _ -> None
