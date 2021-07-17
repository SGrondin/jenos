open! Core_kernel

(* ******************** *)
(* COMMON *)
(* ******************** *)
module type S = sig
  type t [@@deriving sexp, compare, equal, yojson]

  val to_string : t -> string
end

module type S_int = sig
  include S

  module Map : Map.S with type Key.t = t

  val of_int : int -> t

  val to_int64 : t -> int64
end

(* ******************** *)
(* Transport data types *)
(* ******************** *)
module Private = struct
  module Boolean = struct
    type t = bool [@@deriving sexp, compare, equal]

    let of_yojson : Yojson.Safe.t -> (t, string) result = function
    | `String "y" -> Ok true
    | `String "n" -> Ok false
    | j -> Error (sprintf !"Invalid pseudo-Boolean: %{Yojson.Safe}" j)

    let to_string x = if x then "y" else "n"

    let to_yojson x : Yojson.Safe.t = `String (to_string x)
  end

  module DateTime = struct
    type t = Time.t [@@deriving compare, equal]

    let zone = Time.Zone.utc

    let half_day = Time.Span.of_hr 12.0

    let of_string s =
      match String.split ~on:' ' s with
      | [ sdate; stime; ampm ] ->
        let date = Date.of_string sdate in
        let time = Time.Ofday.of_string stime in
        let ofday =
          match (Time.Ofday.to_parts time).hr, ampm with
          | 12, "AM" -> Time.Ofday.sub time half_day |> Option.value_exn ~here:[%here]
          | _, "AM" -> time
          | 12, "PM" -> time
          | _, "PM" -> Time.Ofday.add time half_day |> Option.value_exn ~here:[%here]
          | _, s -> failwithf "Invalid AM/PM: '%s'" s ()
        in
        Time.of_date_ofday ~zone date ofday
      | ll -> failwithf !"Invalid datetime string: %{sexp: string list}" ll ()

    let to_string x = Time.to_string_abs ~zone x

    let sexp_of_t x = Sexp.Atom (to_string x)

    let t_of_sexp = function
    | Sexp.Atom s -> of_string s
    | sexp -> failwithf !"Invalid S-Exp for DateTime: %{Sexp}" sexp ()

    let of_yojson : Yojson.Safe.t -> (t, string) result = function
    | `String s -> Ok (of_string s)
    | j -> Error (sprintf !"Invalid DateTime: %{Yojson.Safe}" j)

    let to_yojson x : Yojson.Safe.t = `String (to_string x)
  end

  module IntStr = struct
    type t = int64 [@@deriving sexp, compare, equal]

    module Map = Int64.Map

    let of_yojson : Yojson.Safe.t -> (t, string) result = function
    | `Intlit x
     |`String x ->
      Ok (Int64.of_string x)
    | `Int x -> Ok (Int.to_int64 x)
    | j -> Error (sprintf !"Invalid pseudo-Int: %{Yojson.Safe}" j)

    let to_string = Int64.to_string

    let to_yojson x : Yojson.Safe.t = `String (to_string x)

    let of_int = Int.to_int64

    let to_int64 = Fn.id
  end

  module IntOpt = struct
    module Self = struct
      type t = int option [@@deriving sexp, compare, equal]
    end

    module Map = Map.Make (Self)
    include Self

    let of_yojson : Yojson.Safe.t -> (t, string) result = function
    | `Int 0 -> Ok None
    | `Int x -> Ok (Some x)
    | j -> Error (sprintf !"Invalid pseudo-IntOption: %{Yojson.Safe}" j)

    let to_string = sprintf !"%{sexp: int option}"

    let to_yojson : t -> Yojson.Safe.t = function
    | None -> `Null
    | Some x -> `Int x
  end

  module StrOpt = struct
    type t = string option [@@deriving sexp, compare, equal]

    let of_yojson : Yojson.Safe.t -> (t, string) result = function
    | `String "" -> Ok None
    | `String s -> Ok (Some s)
    | j -> Error (sprintf !"Invalid pseudo-StringOption: %{Yojson.Safe}" j)

    let to_string = sprintf !"%{sexp: string option}"

    let to_yojson : t -> Yojson.Safe.t = function
    | None -> `Null
    | Some s -> `String s
  end
end

(** [bool] encoded as y/n *)
module Boolean : S with type t = bool = Private.Boolean

(** Time (UTC) formatted as M/D/YYYY HH:MM:SS AM/PM *)
module DateTime : S with type t = Time.t = Private.DateTime

(** [int] sometimes encoded as string *)
module IntStr : S with type t = int64 = Private.IntStr

(** [int option] where 0 indicates None *)
module IntOpt : S with type t = int option = Private.IntOpt

(** [string option] where "" indicates None *)
module StrOpt : S with type t = string option = Private.StrOpt

(* ******************** *)
(* Safe integers *)
(* ******************** *)
module Player_id : S_int = Private.IntStr

module Match_id : S_int = Private.IntStr

module Seconds : S_int = struct
  type t = int [@@deriving sexp, compare, equal, yojson]

  module Map = Int.Map

  let to_string x =
    let minutes = Int.( /% ) x 60 in
    let seconds = x - (minutes * 60) in
    if minutes = 0 then sprintf "%ds" seconds else sprintf "%dm%ds" minutes seconds

  let of_int = Fn.id

  let to_int64 = Int.to_int64
end
