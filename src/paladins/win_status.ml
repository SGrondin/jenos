open! Core_kernel

type t =
  | Winner
  | Loser
[@@deriving sexp, compare, equal]

let to_string = function
| Winner -> "Winner"
| Loser -> "Loser"

let of_string = function
| "Winner" -> Winner
| "Loser" -> Loser
| s -> failwithf "Invalid Win Status: %s" s ()

let to_yojson x = `String (to_string x)

let of_yojson : Yojson.Safe.t -> (t, string) result = function
| `String s -> Ok (of_string s)
| json -> Error (sprintf !"Invalid Win Status json: %{Yojson.Safe}" json)
