open! Core_kernel

type session_start_limit = {
  total: int;
  remaining: int;
  reset_after: int;
  max_concurrency: int;
}
[@@deriving sexp, fields, yojson]

type t = {
  url: Call.Uri.t;
  shards: int;
  session_start_limit: session_start_limit;
}
[@@deriving sexp, fields, yojson]

val bot : token:string -> t Lwt.t
