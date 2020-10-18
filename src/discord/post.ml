open! Core_kernel
open! Cohttp
open! Cohttp_lwt_unix
module Body = Cohttp_lwt.Body

module Payload = struct
  type t = {
    content: string;
    nonce: string;
    tts: bool;
  }
  [@@deriving sexp, fields, to_yojson]
end

let send ~token ~channel_id ~content =
  let body =
    {
      content;
      nonce = Time_now.nanoseconds_since_unix_epoch () |> Int63.to_string;
      tts = false;
    }
    |> Payload.to_yojson
    |> Yojson.Safe.to_string
    |> Body.of_string
  in
  let headers = Header.add (Rest.headers ~token) "content-type" "application/json" in
  let uri = Rest.make_uri ["channels"; channel_id; "messages"] in
  Rest.call ~headers ~body ~f:(fun _ -> ()) `POST uri
