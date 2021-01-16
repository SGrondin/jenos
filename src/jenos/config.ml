open! Core_kernel
open Discord

module Matcher = struct
  type t = Regex.t [@@deriving sexp, to_yojson]

  let of_yojson : Yojson.Safe.t -> (t, string) result = function
  | `Assoc [ ("regex", j) ] -> Regex.of_yojson j
  | `Assoc [ ("chars", j) ] ->
    let open Result.Let_syntax in
    let%bind chars = [%of_yojson: char list] j in
    let word = List.map chars ~f:(sprintf {|(?:%c\s*?)+|}) |> String.concat in
    Result.try_with (fun () -> Regex.create (sprintf {|(?:[^a-z]|^)%s(?:[^a-z]|$)|} word))
    |> Result.map_error ~f:Exn.to_string
  | j -> failwithf "Invalid json for config matcher: %s" (Yojson.Safe.to_string j) ()
end

type reaction = {
  matcher: Matcher.t;
  emojis: Data.Basics.Reference.emoji list;
}
[@@deriving sexp, yojson]

type thresholds = {
  p_common: int;
  p_uncommon: int;
}
[@@deriving sexp, yojson]

type lines = {
  common: string array;
  uncommon: string array;
  rare: string array;
}
[@@deriving sexp, yojson]

type t = {
  token: string;
  activity_type: Data.Activity.Type.t option; [@default None]
  activity_name: string option;
  reactions: reaction list;
  thresholds: thresholds;
  line2: lines;
  line4: lines;
  vc_channel: Basics.Snowflake.t;
  text_channel: Basics.Snowflake.t;
  poll_user_id: Basics.Snowflake.t;
}
[@@deriving sexp, yojson]

let%expect_test "Matcher of Yojson chars" =
  let m =
    Yojson.Safe.from_string {|{"chars": ["r","e","d","x"]}|} |> Matcher.of_yojson |> Result.ok_or_failwith
  in
  Matcher.sexp_of_t m |> Sexp.to_string_hum |> print_endline;
  [%expect {| "(?:[^a-z]|^)(?:r\\s*?)+(?:e\\s*?)+(?:d\\s*?)+(?:x\\s*?)+(?:[^a-z]|$)" |}];
  Regex.matching m "r e d x" |> Bool.to_string |> print_endline;
  [%expect {| true |}]
