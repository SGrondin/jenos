open! Core_kernel
open! Cohttp
open! Cohttp_lwt_unix
module Body = Cohttp_lwt.Body
open! Lwt.Infix

module Payload = struct
  type t = {
    content: string;
    nonce: string;
    tts: bool;
  }
  [@@deriving sexp, fields, to_yojson]
end

let create_message ~token ~channel_id ~content =
  let body =
    {
      content;
      nonce = Latch.Time.get () |> Int64.to_string;
      tts = false;
    }
    |> Payload.to_yojson
    |> Yojson.Safe.to_string
    |> Body.of_string
  in
  let headers = Header.add (Call.headers ~token) "content-type" "application/json" in
  let uri = Call.make_uri ["channels"; channel_id; "messages"] in
  Call.run ~headers ~body `POST uri Print

let create_reaction ~token ~channel_id ~message_id ~emoji =
  let headers = Call.headers ~token in
  let uri = Call.make_uri [
      "channels"; channel_id;
      "messages"; message_id;
      "reactions"; Uri.pct_encode emoji;
      "@me"
    ]
  in
  Call.run ~headers ~expect:204 `PUT uri Ignore
