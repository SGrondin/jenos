open! Core_kernel

type type_ =
| DEFAULT
| RECIPIENT_ADD
| RECIPIENT_REMOVE
| CALL
| CHANNEL_NAME_CHANGE
| CHANNEL_ICON_CHANGE
| CHANNEL_PINNED_MESSAGE
| GUILD_MEMBER_JOIN
| USER_PREMIUM_GUILD_SUBSCRIPTION
| USER_PREMIUM_GUILD_SUBSCRIPTION_TIER_1
| USER_PREMIUM_GUILD_SUBSCRIPTION_TIER_2
| USER_PREMIUM_GUILD_SUBSCRIPTION_TIER_3
| CHANNEL_FOLLOW_ADD
| GUILD_DISCOVERY_DISQUALIFIED
| GUILD_DISCOVERY_REQUALIFIED
[@@deriving sexp]
let type__of_yojson = function
| `Int 0 -> Ok DEFAULT
| `Int 1 -> Ok RECIPIENT_ADD
| `Int 2 -> Ok RECIPIENT_REMOVE
| `Int 3 -> Ok CALL
| `Int 4 -> Ok CHANNEL_NAME_CHANGE
| `Int 5 -> Ok CHANNEL_ICON_CHANGE
| `Int 6 -> Ok CHANNEL_PINNED_MESSAGE
| `Int 7 -> Ok GUILD_MEMBER_JOIN
| `Int 8 -> Ok USER_PREMIUM_GUILD_SUBSCRIPTION
| `Int 9 -> Ok USER_PREMIUM_GUILD_SUBSCRIPTION_TIER_1
| `Int 10 -> Ok USER_PREMIUM_GUILD_SUBSCRIPTION_TIER_2
| `Int 11 -> Ok USER_PREMIUM_GUILD_SUBSCRIPTION_TIER_3
| `Int 12 -> Ok CHANNEL_FOLLOW_ADD
| `Int 14 -> Ok GUILD_DISCOVERY_DISQUALIFIED
| `Int 15 -> Ok GUILD_DISCOVERY_REQUALIFIED
| json -> Error (sprintf "Impossible to parse JSON %s into a message type" (Yojson.Safe.to_string json))

type t = {
  id: string;
  channel_id: string;
  author: User.t;
  member: Channel.member option [@default None];
  content: string;
  type_: type_ [@key "type"];
}
[@@deriving sexp, fields, of_yojson { exn = true; strict = false }]
