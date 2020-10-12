open! Core_kernel
open! Cohttp
open! Cohttp_lwt_unix
module Body = Cohttp_lwt.Body

let previous = ref (Int64.of_int 0)
let cooldown = Int64.(20L * 60L * 1_000_000_000L)

module Payload = struct
  type t = {
    content: string;
    nonce: string;
    tts: bool;
  }
  [@@deriving sexp, fields, to_yojson]
end

let can_send () =
  let open Int64 in
  let now = Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64 in
  let b = now > (!previous + cooldown) in
  if b then previous := now;
  b

let send ~channel_id ~content =
  if can_send () then begin
    let body =
      let nonce = Time_now.nanoseconds_since_unix_epoch () |> Int63.to_string in
      { content; nonce; tts = false }
      |> Payload.to_yojson
      |> Yojson.Safe.to_string
      |> Body.of_string
    in
    let headers = Header.add Rest.headers "content-type" "application/json" in
    let uri = Rest.make_uri ["channels"; channel_id; "messages"] in
    Rest.call ~headers ~body ~f:(fun _ -> ()) `POST uri
  end else Lwt_io.printl "⏳ Waiting until we can send again"
