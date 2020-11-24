open! Core_kernel

type t [@@deriving sexp_of, of_yojson]

val of_string: string -> t

val matching: t -> string -> bool

val exec: t -> string -> string array option

