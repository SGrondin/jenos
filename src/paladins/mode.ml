open! Core_kernel

type t =
  | Siege
  | TDM
  | Onslaught
  | Ranked_KBM
  | Ranked_Gamepad
[@@deriving sexp, compare, equal]

let to_id = function
| Siege -> 424
| TDM -> 469
| Onslaught -> 452
| Ranked_KBM -> 486
| Ranked_Gamepad -> 428

let of_id = function
| 424 -> Siege
| 469 -> TDM
| 452 -> Onslaught
| 486 -> Ranked_KBM
| 428 -> Ranked_Gamepad
| x -> failwithf "Invalid Mode id: %d" x ()

let to_yojson x = `Int (to_id x)

let of_yojson : Yojson.Safe.t -> (t, string) result = function
| `Int x -> Ok (of_id x)
| json -> Error (sprintf !"Invalid Mode json: %{Yojson.Safe}" json)
