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
[@@deriving sexp, yojson]

type t = {
  id: string;
  channel_id: string;
  author: User.t;
  member: Channel.member option;
  content: string;
  type_: type_;
}
[@@deriving sexp, fields, yojson]
