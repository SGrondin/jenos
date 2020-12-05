open! Core_kernel
open! Cohttp
open! Cohttp_lwt_unix
module Body = Cohttp_lwt.Body

type session_start_limit = {
  total: int;
  remaining: int;
  reset_after: int;
  max_concurrency: int;
}
[@@deriving sexp, fields, yojson { strict = false }]

type t = {
  url: Call.Uri.t;
  shards: int;
  session_start_limit: session_start_limit;
}
[@@deriving sexp, fields, yojson { strict = false }]

let bot ~token =
  let uri = Call.make_uri [ "gateway"; "bot" ] in
  Call.run ~headers:(Call.headers ~token) `GET uri (Parse of_yojson)
