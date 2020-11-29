open! Core_kernel

type t [@@deriving sexp, yojson]

val create : string -> t

val matching : t -> string -> bool

val exec : t -> string -> string array option
