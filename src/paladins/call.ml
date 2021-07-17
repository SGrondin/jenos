open! Core_kernel
open! Cohttp
open! Cohttp_lwt_unix
module Body = Cohttp_lwt.Body
open Lwt.Infix

module type S = sig
  val dev_id : string

  val auth_key : string
end

module HZTS : sig
  type t [@@deriving sexp, compare, equal]

  val now : unit -> t

  val to_string : t -> string
end = struct
  type t = int * int * int * int * int * int [@@deriving sexp, compare, equal]

  let now () =
    let date, ofday = Time.now () |> Time.to_date_ofday ~zone:Time.Zone.utc in
    let time = Time.Ofday.to_parts ofday in
    Date.year date, Date.month date |> Month.to_int, Date.day date, time.hr, time.min, time.sec

  let to_string (y, m, d, hh, mm, dd) = sprintf "%d%02d%02d%02d%02d%02d" y m d hh mm dd
end

let headers = Header.init_with "User-Agent" "Camlbot"

let base_uri = Uri.make ~scheme:"https" ~host:"api.paladins.com" ()

let signature ~dev_id ~auth_key endpoint hzts =
  sprintf !"%s%s%s%{HZTS}" dev_id endpoint auth_key hzts |> Md5.digest_string |> Md5.to_hex

let make_uri ~dev_id ~auth_key ?(uri = base_uri) ~endpoint ~session extras =
  let hzts = HZTS.now () in
  let ll =
    "paladinsapi.svc"
    :: sprintf "%sJson" endpoint
    :: dev_id
    :: signature ~dev_id ~auth_key endpoint hzts
    :: session
    :: HZTS.to_string hzts
    :: extras
  in
  ll |> String.concat ~sep:"/" |> Uri.with_path uri

let latch = Latch.(create ~cooldown:(Time.ms 500L))

type body = JSON of Yojson.Safe.t [@@unboxed]

type _ handler =
  | Unparsed : string handler
  | Ignore : unit handler
  | Parse     : (Yojson.Safe.t -> ('a, string) result) -> 'a handler
  | Parse_exn : (Yojson.Safe.t -> 'a) -> 'a handler

let run :
   type a. ?expect:int -> ?body:body -> Code.meth -> Uri.t -> ?print_body:bool -> a handler -> a Lwt.t =
 fun ?(expect = 200) ?body meth uri ?(print_body = false) handler ->
  let%lwt () = Latch.wait_and_trigger latch in
  let body, headers =
    match body with
    | None as x -> x, headers
    | Some (JSON x) ->
      ( Some (Yojson.Safe.to_string x |> Body.of_string),
        Header.add headers "content-type" "application/json" )
  in
  let%lwt res, res_body = Client.call ~headers ?body meth uri in
  let status = Response.status res in
  let get_body_str = lazy (Body.to_string res_body) in
  let%lwt () = if print_body then force get_body_str >>= Lwt_io.printl else Lwt.return_unit in
  match Code.code_of_status status with
  | code when code = expect ->
    let handle : a Lwt.t =
      match handler with
      | Unparsed -> force get_body_str
      | Ignore -> Body.drain_body res_body
      | Parse f -> (
        force get_body_str >|= fun s ->
        Yojson.Safe.from_string s |> f |> function
        | Ok x -> x
        | Error msg ->
          print_endline s;
          failwith msg
      )
      | Parse_exn f -> force get_body_str >|= fun s -> Yojson.Safe.from_string s |> f
    in
    handle
  | _ ->
    let%lwt body_str = force get_body_str in
    failwithf
      !"Invalid HTTP response (%{Code.string_of_status})\n%{Header}\n%s"
      status (Response.headers res) body_str ()
