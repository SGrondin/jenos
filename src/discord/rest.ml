open! Core_kernel
open! Cohttp
open! Cohttp_lwt_unix
module Body = Cohttp_lwt.Body

let token = "NzYxOTgwNjI4MDQ4NTQzNzU1.X3ifwg.G8Ebn0kkIXMmrGjySDTmyfrwZHA"
let name = "Camlbot"
let headers = Header.of_list [
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
