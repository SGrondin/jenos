open! Core_kernel

module Self : sig
  type t [@@deriving sexp, yojson]

  val create : string -> t

  val matching : t -> string -> bool

  val exec : t -> string -> string array option

  val replace : t -> f:(Re.Group.t -> string) -> string -> string
end

include module type of Self with type t = Self.t

module Matcher : sig
  type t = Self.t [@@deriving sexp, yojson]
end

module Replacement : sig
  type t = Self.t [@@deriving sexp, yojson]
end
