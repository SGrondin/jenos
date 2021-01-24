open! Core_kernel
open Discord

module Add_reactions_config = struct
  type reaction = {
    matcher: Regex.Matcher.t;
    emojis: Data.Basics.Reference.emoji list;
  }
  [@@deriving sexp, of_yojson]

  type t = reaction list [@@deriving sexp, of_yojson]
end

module Track_vc_config = struct
  type thresholds = {
    p_common: int;
    p_uncommon: int;
  }
  [@@deriving sexp, of_yojson]

  type lines = {
    thresholds: thresholds;
    common: string array;
    uncommon: string array;
    rare: string array;
  }
  [@@deriving sexp, of_yojson]

  type t = {
    vc_channel: Basics.Snowflake.t;
    text_channel: Basics.Snowflake.t;
    lines2: lines;
    lines4: lines;
  }
  [@@deriving sexp, of_yojson]
end

module Send_curses_config = struct
  type trigger =
    | Phrase of string
    | Word   of string
  [@@deriving sexp]

  let trigger_of_yojson = function
  | `Assoc [ ("phrase", `String s) ] -> Ok (Phrase s)
  | `Assoc [ ("word", `String s) ] -> Ok (Word s)
  | j -> Error (sprintf !"Invalid JSON for curse trigger: %{Yojson.Safe}" j)

  type t = {
    cursed_channel: Basics.Snowflake.t;
    subreddits: string list;
    triggers: trigger list;
    phrases: string array;
    phrase_empty: string;
  }
  [@@deriving sexp, of_yojson]
end

module Make_meeting_poll_config = struct
  type t = { poll_user_id: Basics.Snowflake.t } [@@deriving sexp, of_yojson] [@@unboxed]
end

type t = {
  token: string;
  activity_type: Data.Activity.Type.t option; [@default None]
  activity_name: string option;
  add_reactions: Add_reactions_config.t;
  track_vc: Track_vc_config.t;
  send_curses: Send_curses_config.t;
  make_meeting_poll: Make_meeting_poll_config.t;
}
[@@deriving sexp, of_yojson]
