open! Core_kernel
open Discord

type reaction = {
  matcher: Regex.Matcher.t;
  emojis: Data.Basics.Reference.emoji list;
}
[@@deriving sexp, yojson]

module Lines = struct
  type thresholds = {
    p_common: int;
    p_uncommon: int;
  }
  [@@deriving sexp, yojson]

  type t = {
    thresholds: thresholds;
    common: string array;
    uncommon: string array;
    rare: string array;
  }
  [@@deriving sexp, yojson]
end

type t = {
  token: string;
  activity_type: Data.Activity.Type.t option; [@default None]
  activity_name: string option;
  reactions: reaction list;
  lines2: Lines.t;
  lines4: Lines.t;
  phrases: string array;
  phrase_empty: string;
  subreddits: string list;
  vc_channel: Basics.Snowflake.t;
  text_channel: Basics.Snowflake.t;
  cursed_channel: Basics.Snowflake.t;
  poll_user_id: Basics.Snowflake.t;
}
[@@deriving sexp, yojson]
