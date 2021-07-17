open! Core_kernel
open Lwt.Infix

type getplayeridbyname = {
  name: string; [@key "Name"]
  player_id: Types.Player_id.t;
  portal: string;
  portal_id: Types.IntStr.t;
  privacy_flag: Types.Boolean.t;
  ret_msg: string option; [@default None]
}
[@@deriving sexp, compare, equal, yojson { strict = false }]

module Make (M : Call.S) = struct
  let make_uri = Call.make_uri ~dev_id:M.dev_id ~auth_key:M.auth_key

  let get_player_id_by_name ~session ~name =
    let uri = make_uri ~endpoint:"getplayeridbyname" ~session [ name ] in
    Call.run `GET uri (Parse [%of_yojson: getplayeridbyname list]) >|= List.hd

  (** Stats for that player, last 50 matches *)
  let get_match_history ~session ~player_id =
    let uri = make_uri ~endpoint:"getmatchhistory" ~session [ Types.Player_id.to_string player_id ] in
    Call.run `GET uri Unparsed

  (** Stats for that player, by champion, for a specific queue *)
  let get_queue_stats ~session ~player_id ~queue =
    let uri =
      make_uri ~endpoint:"getqueuestats" ~session
        [ Types.Player_id.to_string player_id; Mode.to_id queue |> Int.to_string ]
    in
    Call.run `GET uri Unparsed
end
