open! Core_kernel

type t

val create: cooldown:Int64.t -> t

val check: t -> Int64.t -> bool

val trigger: t -> Int64.t -> unit
