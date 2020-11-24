open! Core_kernel
open! Cohttp
open! Cohttp_lwt_unix
module Body = Cohttp_lwt.Body
open Lwt.Infix

let name = "Camlbot v0.9"

let headers ~token = Header.of_list [
    "Authorization", sprintf "Bot %s" token;
    "User-Agent", name;
  ]

module Uri = struct
  include Uri

  let t_of_sexp = function
  | Sexp.Atom s -> of_string s
  | sexp -> failwithf "Impossible to parse S-Exp %s into an URI" (Sexp.to_string sexp) ()
  let sexp_of_t uri = Sexp.Atom (to_string uri)
  let to_yojson uri : Yojson.Safe.t = `String (to_string uri)
  let of_yojson : Yojson.Safe.t -> (t, string) result = function
  | `String s -> Ok (of_string s)
  | json -> Error (sprintf "Impossible to parse JSON %s into an URI" (Yojson.Safe.to_string json))
end

(* https://discord.com/api/v8/ *)
let base_uri = Uri.make ~scheme:"https" ~host:"discord.com" ()
let make_uri ?(uri = base_uri) ll =
  ("api" :: "v8" :: ll)
  |> String.concat ~sep:"/"
  |> Uri.with_path uri

let latch = Latch.(create ~cooldown:Int64.(400L * Time.ms))

type _ handler =
| JSON : Yojson.Safe.t handler
| Ignore : unit handler
| Parse : (Yojson.Safe.t -> 'a) -> 'a handler

let run : type a. headers:Header.t -> ?expect:int -> ?body:Body.t -> Code.meth -> Uri.t -> ?print_body:bool -> a handler -> a Lwt.t =
  fun ~headers ?(expect = 200) ?body meth uri ?(print_body = false) handler ->
  let%lwt () = Latch.wait_and_trigger latch in
  let%lwt res, res_body = Client.call ~headers ?body meth uri in
  let status = Response.status res in
  let get_body_str = lazy (Body.to_string res_body) in
  let%lwt () =
    if print_body
    then force get_body_str >>= Lwt_io.printl
    else Lwt.return_unit
  in
  begin match Code.code_of_status status with
  | code when code = expect ->
    let handle : a Lwt.t = begin match handler with
    | JSON ->
      force get_body_str >|= Yojson.Safe.from_string
    | Ignore ->
      Body.drain_body res_body
    | Parse f ->
      force get_body_str >|= Yojson.Safe.from_string >|= f
    end
    in
    handle
  | _ ->
    let%lwt body_str = force get_body_str in
    failwithf "Invalid HTTP response (%s)\n%s\n%s"
      (Code.string_of_status status) (Response.headers res |> Header.to_string) body_str ()
  end
