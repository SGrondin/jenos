open! Core_kernel

type t = Re.re * string

let create s = Re.Perl.re ~opts:[`Caseless] s |> Re.Perl.compile, s

let sexp_of_t (_re, s) = Sexp.Atom s
let t_of_sexp = function
| Sexp.Atom s -> create s
| sexp -> failwithf "Invalid S-Exp for a Regex: %s" (Sexp.to_string sexp) ()

let to_yojson (_re, s) = `String s

let of_yojson = function
| `String s -> Result.try_with (fun () -> create s) |> Result.map_error ~f:Exn.to_string
| json -> Error (sprintf "Impossible to parse JSON %s into a regex type" (Yojson.Safe.to_string json))

let matching (re, _s) str = Re.execp re str

let exec (re, _s) str = Re.exec_opt re str |> Option.map ~f:Re.Group.all
