open! Core_kernel

type t = Re.re * string

let sexp_of_t (_re, s) = Sexp.Atom s

let of_string s = Re.Perl.re ~opts:[`Caseless] s |> Re.Perl.compile, s

let of_yojson = function
| `String s -> Result.try_with (fun () -> of_string s) |> Result.map_error ~f:Exn.to_string
| json -> Error (sprintf "Impossible to parse JSON %s into a regex type" (Yojson.Safe.to_string json))

let matching (re, _s) str = Re.execp re str

let exec (re, _s) str = Re.exec_opt re str |> Option.map ~f:Re.Group.all
