open! Core_kernel
open Lwt.Syntax

type createsession = {
  ret_msg: string option; [@default None]
  session_id: string;
  timestamp: string;
}
[@@deriving sexp, compare, equal, yojson { strict = false }]

let latch = Latch.(create ~cooldown:(Time.min 14L))

type status =
  | Blank
  | Fetching of string Lwt.t

module Make (M : Call.S) = struct
  let make_uri = Call.make_uri ~dev_id:M.dev_id ~auth_key:M.auth_key

  let current = ref Blank

  let create_manually () =
    let uri =
      let endpoint = "createsession" in
      let hzts = Call.HZTS.now () in
      let ll =
        [
          "paladinsapi.svc";
          sprintf "%sJson" endpoint;
          M.dev_id;
          Call.signature ~dev_id:M.dev_id ~auth_key:M.auth_key endpoint hzts;
          Call.HZTS.to_string hzts;
        ]
      in
      ll |> String.concat ~sep:"/" |> Uri.with_path Call.base_uri
    in
    let+ { session_id; _ } = Call.run `GET uri (Parse [%of_yojson: createsession]) in
    session_id

  let get () =
    let now = Latch.Time.get () in
    match !current with
    | Fetching p when not @@ Latch.check ~now latch -> p
    | Blank
     |Fetching _ ->
      Latch.trigger ~now latch;
      let p = create_manually () in
      current := Fetching p;
      p

  let get_data_used ~session =
    let uri = make_uri ~endpoint:"getdataused" ~session [] in
    Call.run `GET uri Unparsed
end
