open! Core_kernel

module Self = struct
  type t = Re.re * string

  let create s = Re.Perl.re ~opts:[ `Caseless ] s |> Re.Perl.compile, s

  let sexp_of_t (_re, s) = Sexp.Atom s

  let t_of_sexp = function
  | Sexp.Atom s -> create s
  | sexp -> failwithf "Invalid S-Exp for a Regex: %s" (Sexp.to_string sexp) ()

  let to_yojson (_re, s) = `String s

  let of_yojson = function
  | `String s -> Result.try_with (fun () -> create s) |> Result.map_error ~f:Exn.to_string
  | json -> Error (sprintf "Impossible to parse JSON '%s' into a regex type" (Yojson.Safe.to_string json))

  let matching (re, _s) str = Re.execp re str

  let exec (re, _s) str = Re.exec_opt re str |> Option.map ~f:Re.Group.all

  let replace (re, _s) ~f str = Re.replace re ~f str
end

include Self

module Matcher = struct
  type t = Self.t [@@deriving sexp, to_yojson]

  let of_yojson : Yojson.Safe.t -> (t, string) result = function
  | `Assoc [ ("regex", j) ] -> [%of_yojson: Self.t] j
  | `Assoc [ ("chars", j) ] ->
    let open Result.Let_syntax in
    let%bind chars = [%of_yojson: char list] j in
    let word = List.map chars ~f:(sprintf {|(?:%c\s*?)+|}) |> String.concat in
    Result.try_with (fun () -> Self.create (sprintf {|(?:[^a-z]|^)%s(?:[^a-z]|$)|} word))
    |> Result.map_error ~f:Exn.to_string
  | j -> failwithf "Invalid json for config matcher: %s" (Yojson.Safe.to_string j) ()
end

module Replacement = struct
  type t = Self.t [@@deriving sexp, to_yojson]

  let of_yojson : Yojson.Safe.t -> (t, string) result = function
  | `String s -> Ok (Self.create (sprintf "([^a-z]|^)(%s)([^a-z]|$)" s))
  | j -> Error (sprintf "Invalid json for config matcher: %s" (Yojson.Safe.to_string j))
end

let%expect_test "Matcher of Yojson chars" =
  let m =
    Yojson.Safe.from_string {|{"chars": ["r","e","d","x"]}|}
    |> [%of_yojson: Matcher.t]
    |> Result.ok_or_failwith
  in
  [%sexp_of: Matcher.t] m |> Sexp.to_string_hum |> print_endline;
  [%expect {| "(?:[^a-z]|^)(?:r\\s*?)+(?:e\\s*?)+(?:d\\s*?)+(?:x\\s*?)+(?:[^a-z]|$)" |}];
  matching m "r e d x" |> Bool.to_string |> print_endline;
  [%expect {| true |}]

let%expect_test "Replace strings" =
  let test s =
    let regex = `String "bad" |> [%of_yojson: Replacement.t] |> Result.ok_or_failwith in
    replace regex s ~f:(fun g ->
        match Re.Group.all g with
        | [| _whole; left; word; right |] ->
          sprintf "%s%s%s" left (String.make (String.length word) '*') right
        | _ -> failwith "Invalid number of matches in regex replacement")
    |> print_endline
  in
  test "foo bad bar";
  [%expect {| foo *** bar |}];
  test "foobadbar";
  [%expect {| foobadbar |}];
  test "foo-BAD bar";
  [%expect {| foo-*** bar |}]
