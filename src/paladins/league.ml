open! Core_kernel

type t =
  | Bronze_V
  | Bronze_IV
  | Bronze_III
  | Bronze_II
  | Bronze_I
  | Silver_V
  | Silver_IV
  | Silver_III
  | Silver_II
  | Silver_I
  | Gold_V
  | Gold_IV
  | Gold_III
  | Gold_II
  | Gold_I
  | Platinum_V
  | Platinum_IV
  | Platinum_III
  | Platinum_II
  | Platinum_I
  | Diamond_V
  | Diamond_IV
  | Diamond_III
  | Diamond_II
  | Diamond_I
  | Masters_I
  | Grandmaster
[@@deriving sexp, compare, equal]

let to_id = function
| Bronze_V -> 1
| Bronze_IV -> 2
| Bronze_III -> 3
| Bronze_II -> 4
| Bronze_I -> 5
| Silver_V -> 6
| Silver_IV -> 7
| Silver_III -> 8
| Silver_II -> 9
| Silver_I -> 10
| Gold_V -> 11
| Gold_IV -> 12
| Gold_III -> 13
| Gold_II -> 14
| Gold_I -> 15
| Platinum_V -> 16
| Platinum_IV -> 17
| Platinum_III -> 18
| Platinum_II -> 19
| Platinum_I -> 20
| Diamond_V -> 21
| Diamond_IV -> 22
| Diamond_III -> 23
| Diamond_II -> 24
| Diamond_I -> 25
| Masters_I -> 26
| Grandmaster -> 27

let of_id = function
| 1 -> Bronze_V
| 2 -> Bronze_IV
| 3 -> Bronze_III
| 4 -> Bronze_II
| 5 -> Bronze_I
| 6 -> Silver_V
| 7 -> Silver_IV
| 8 -> Silver_III
| 9 -> Silver_II
| 10 -> Silver_I
| 11 -> Gold_V
| 12 -> Gold_IV
| 13 -> Gold_III
| 14 -> Gold_II
| 15 -> Gold_I
| 16 -> Platinum_V
| 17 -> Platinum_IV
| 18 -> Platinum_III
| 19 -> Platinum_II
| 20 -> Platinum_I
| 21 -> Diamond_V
| 22 -> Diamond_IV
| 23 -> Diamond_III
| 24 -> Diamond_II
| 25 -> Diamond_I
| 26 -> Masters_I
| 27 -> Grandmaster
| x -> failwithf "Invalid League id: %d" x ()

let to_yojson x = `Int (to_id x)

let of_yojson : Yojson.Safe.t -> (t, string) result = function
| `Int x -> Ok (of_id x)
| json -> Error (sprintf !"Invalid League json: %{Yojson.Safe}" json)
