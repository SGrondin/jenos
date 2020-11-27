open! Core_kernel

type counter = { count: int; ack: int }

exception Discontinuity_error of counter

type t

type heartbeat_loop = {
  interval: int;
  respond: Protocol.Send.t -> unit Lwt.t;
  cancel: Websocket.Frame.t Lwt.u;
}

val create : unit -> t

val received_hello : heartbeat_loop -> t -> t

val received_ready : session_id:string -> t -> t

val received_seq : int option -> t -> unit

val received_ack : t -> unit

val terminate : t -> unit

val seq : t -> int option

val session_id : t -> string option
