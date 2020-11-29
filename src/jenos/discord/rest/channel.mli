open! Core_kernel

val create_message : token:string -> channel_id:string -> content:string -> 'a Call.handler -> 'a Lwt.t

val delete_message : token:string -> channel_id:string -> message_id:string -> unit Lwt.t

val create_reaction :
  token:string -> channel_id:string -> message_id:string -> emoji:string -> 'a Call.handler -> 'a Lwt.t
