open! Core_kernel

module Open : sig
  type 'a state = {
    internal_state: Internal_state.t;
    user_state: 'a;
  }

  type 'a router_action =
  | R_Forward of 'a state
  | R_Reconnect of 'a state
  | R_Reidentify of 'a state
end

open Open

val forward : 'a state -> 'a router_action Lwt.t
val reconnect : 'a state -> 'a router_action Lwt.t
val reidentify : 'a state -> 'a router_action Lwt.t

type send = (Websocket.Frame.t -> unit Lwt.t)

val identify : Login.t -> send -> unit Lwt.t

val handle_message :
  Login.t ->
  send:send ->
  cancel:Websocket.Frame.t Lwt.u ->
  'a state ->
  Message.t ->
  'a router_action Lwt.t
