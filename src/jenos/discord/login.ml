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
[@@deriving sexp, enum, compare]

type t = {
  token: string;
  intents: int;
  activity: Commands.Identify.activity;
  status: Commands.Identify.status;
  afk: bool;
}
[@@deriving sexp]

let create ~token ~intents ?(activity = Commands.Identify.Game "Bot things")
   ?(status = Commands.Identify.Online) ?(afk = false) () =
  {
    token;
    intents =
      List.dedup_and_sort ~compare:compare_intent intents
      |> List.fold ~init:0 ~f:(fun acc x -> acc + (1 lsl intent_to_enum x));
    activity;
    status;
    afk;
  }
