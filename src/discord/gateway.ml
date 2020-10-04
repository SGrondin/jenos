open! Core_kernel
open Cohttp
open Cohttp_lwt_unix
module Body = Cohttp_lwt.Body

type session_start_limit = {
  total: int;
  remaining: int;
  reset_after: int;
  max_concurrency: int;
}
[@@deriving sexp, yojson { exn = true; strict = false }]

type response = {
  url: Rest.Uri.t;
  shards: int;
  session_start_limit: session_start_limit;
}
[@@deriving sexp, yojson { exn = true; strict = false }]

let get () =
  let%lwt res, res_body = Client.get ~headers:Rest.headers (Rest.make_uri ["gateway"; "bot"]) in
  let status = Response.status res in
  let%lwt body_str = Body.to_string res_body in
  print_endline body_str;
  begin match Code.code_of_status status with
  | 200 ->
    let json = Yojson.Safe.from_string body_str in
    Lwt.return (response_of_yojson_exn json)
  | _ ->
    failwithf "Invalid HTTP response (%s)" (Code.string_of_status status) ()
  end
