open! Core_kernel
open Cohttp
open Cohttp_lwt_unix
module Body = Cohttp_lwt.Body

module Payload = struct
  type t = {
    content: string;
    nonce: string;
    tts: bool;
  }
  [@@deriving sexp, yojson]
end

let send ~channel_id ~content =
  let body =
    let nonce = Time_now.nanoseconds_since_unix_epoch () |> Int63.to_string in
    Payload.{ content; nonce; tts = false }
    |> Payload.to_yojson
    |> Yojson.Safe.to_string
    |> Body.of_string
  in
  let headers = Header.add Rest.headers "content-type" "application/json" in
  let uri = Rest.make_uri ["channels"; channel_id; "messages"] in
  let%lwt res, res_body = Client.post ~headers ~body uri in
  let status = Response.status res in
  let%lwt body_str = Body.to_string res_body in
  print_endline body_str;
  begin match Code.code_of_status status with
  | 200 ->
    Lwt.return_unit
  | _ ->
    failwithf "Invalid HTTP response (%s)" (Code.string_of_status status) ()
  end
