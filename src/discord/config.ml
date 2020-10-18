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
[@@deriving enum]


type t = {
  token: string;
  intents: int;
  activity: Commands.Identify.activity;
  status: Commands.Identify.status;
  afk: bool;
}

let create
    ~token
    ~intents
    ?(activity = Commands.Identify.Game "Party Manager")
    ?(status = Commands.Identify.Online)
    ?(afk = false)
    () = {
  token;
  intents = List.fold intents ~init:0 ~f:(fun acc x ->
      acc + (1 lsl (intent_to_enum x)));
  activity;
  status;
  afk;
}
