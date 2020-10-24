open! Core_kernel
open! Cohttp
open! Cohttp_lwt_unix
module Body = Cohttp_lwt.Body

let name = "Camlbot"

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

let exec ~headers ?body ~f meth uri =
  let%lwt res, res_body = Client.call ~headers ?body meth uri in
  let status = Response.status res in
  let%lwt body_str = Body.to_string res_body in
  print_endline body_str;
  begin match Code.code_of_status status with
  | 200 ->
    let json = Yojson.Safe.from_string body_str in
    Lwt.return (f json)
  | _ ->
    failwithf "Invalid HTTP response (%s)" (Code.string_of_status status) ()
  end
