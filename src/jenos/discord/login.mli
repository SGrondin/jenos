open! Core_kernel

type intent =
| GUILDS
| GUILD_MEMBERS
| GUILD_BANS
| GUILD_EMOJIS
| GUILD_INTEGRATIONS
| GUILD_WEBHOOKS
| GUILD_INVITES
| GUILD_VOICE_STATES
| GUILD_PRESENCES
| GUILD_MESSAGES
| GUILD_MESSAGE_REACTIONS
| GUILD_MESSAGE_TYPING
| DIRECT_MESSAGES
| DIRECT_MESSAGE_REACTIONS
| DIRECT_MESSAGE_TYPING
[@@deriving sexp, enum]


type t = {
  token: string;
  intents: int;
  activity: Commands.Identify.activity;
  status: Commands.Identify.status;
  afk: bool;
} [@@deriving sexp]

val create :
  token:string ->
  intents:intent list ->
  ?activity:Commands.Identify.activity ->
  ?status:Commands.Identify.status ->
  ?afk:bool ->
  unit ->
  t
