open! Core_kernel

type t

type heartbeat_loop = {
  on_exn: exn -> unit Lwt.t;
  interval: int;
  respond: Message.Send.t -> unit Lwt.t;
  close: ack:int -> count:int -> unit Lwt.t;
}

val create : unit -> t

val received_hello : heartbeat_loop -> t -> t

val received_ready : session_id:string -> t -> t

val received_seq : int option -> t -> unit

val received_ack : t -> unit

val terminate : t -> unit

val seq : t -> int option

val session_id : t -> string option
