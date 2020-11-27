open! Core_kernel

type _ handler =
| JSON : Yojson.Safe.t handler
| Ignore : unit handler
| Parse : (Yojson.Safe.t -> ('a, string) result) -> 'a handler
| Parse_exn : (Yojson.Safe.t -> 'a) -> 'a handler

module Uri : sig
  type t = Uri.t [@@deriving sexp, yojson]
end

val name : string

val latch : Latch.t

val headers : token:string -> Cohttp.Header.t

val make_uri : ?uri:Uri.t -> string list -> Uri.t

val run :
  headers:Cohttp.Header.t ->
  ?expect:int ->
  ?body:Cohttp_lwt.Body.t ->
  Cohttp.Code.meth ->
  Uri.t ->
  ?print_body:bool ->
  'a handler ->
  'a Lwt.t
