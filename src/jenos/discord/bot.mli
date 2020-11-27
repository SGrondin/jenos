open! Core_kernel

type close =
| Reconnecting
| Final
| Exception of Exn.t

type event =
| Before_action of Message.t
| After_action of Message.t
| Before_reconnecting
| Error_connection_closed
| Error_connection_reset
| Error_connection_timed_out
| Error_discord_server of { extension: int; final: bool; content: string }
| Error_discontinuity of Internal_state.counter

val in_background : on_exn:(exn -> unit Lwt.t) -> (unit -> unit Lwt.t) -> unit

module type S = sig
  type state

  val create : unit -> state
  val on_exn : exn -> unit Lwt.t
  val on_closing_connection : close -> unit Lwt.t
  val on_event : state -> event -> state Lwt.t
end

module Make : functor (Bot : S) -> sig
  val start : Login.t -> unit Lwt.t
end
