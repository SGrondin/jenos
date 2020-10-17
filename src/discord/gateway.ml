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
[@@deriving sexp, fields, of_yojson { exn = true; strict = false }]

type response = {
  url: Rest.Uri.t;
  shards: int;
  session_start_limit: session_start_limit;
}
[@@deriving sexp, fields, of_yojson { exn = true; strict = false }]

let get ~token =
  let uri = Rest.make_uri ["gateway"; "bot"] in
  Rest.call ~headers:(Rest.headers ~token) ~f:response_of_yojson_exn `GET uri
