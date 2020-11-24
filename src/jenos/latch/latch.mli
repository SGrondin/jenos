open! Core_kernel

type t

val create: cooldown:Int64.t -> t

module Time : sig
  val get: unit -> Int64.t
  val min: Int64.t
  val sec: Int64.t
  val ms: Int64.t
end

val check: ?now:Int64.t -> t -> bool

val trigger: ?now:Int64.t -> t -> unit

val wait_and_trigger: ?now:Int64.t -> t -> unit Lwt.t
