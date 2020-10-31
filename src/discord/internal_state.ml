open! Core_kernel

type heartbeat = {
  ms: int;
  until: bool ref;
  seq: int option ref;
}
[@@deriving sexp]

let rec loop sec until seq respond =
  let%lwt () = Lwt_unix.sleep sec in
  if !until then Lwt.return_unit else begin
    Lwt.async (fun () ->
      Lwt.catch (fun () ->
        respond @@ Commands.Heartbeat.to_message !seq
      ) (fun exn -> Lwt_io.eprintlf "Background loop exn: %s. Please report this bug."
          (Exn.to_string exn))
    );
    loop sec until seq respond
  end

let start_heartbeat ~heartbeat_interval ~seq respond =
  let { ms; until; seq } as heartbeat = {
    ms = heartbeat_interval;
    until = ref false;
    seq;
  }
  in
  Lwt.async (fun () -> loop (ms // 1000) until seq respond);
  heartbeat

let stop_heartbeat { until; _ } = until := true

type t =
| Starting of int option ref
| After_hello of heartbeat
| Connected of {
    heartbeat: heartbeat;
    session_id: string;
  }
[@@deriving sexp]

let initial () = Starting (ref None)

let received_hello ~heartbeat_interval respond = function
| Starting seq ->
  After_hello (start_heartbeat ~heartbeat_interval ~seq respond)
| After_hello heartbeat ->
  stop_heartbeat heartbeat;
  After_hello (start_heartbeat ~heartbeat_interval ~seq:heartbeat.seq respond)
| Connected { heartbeat; session_id } ->
  stop_heartbeat heartbeat;
  Connected {
    heartbeat = start_heartbeat ~heartbeat_interval ~seq:heartbeat.seq respond;
    session_id;
  }

let received_ready ~session_id = function
| After_hello heartbeat -> Connected {
    heartbeat;
    session_id;
  }
| (Starting _ as x)
| (Connected _ as x) ->
  failwithf "Invalid internal state transition to_connected: %s. Please report this bug."
    (sexp_of_t x |> Sexp.to_string) ()

let seq = function
| Starting seq
| After_hello { seq; _ }
| Connected { heartbeat = { seq; _ }; _ } -> seq

let heartbeat = function
| Starting _ -> None
| After_hello heartbeat
| Connected { heartbeat; _ } -> Some heartbeat

let session_id = function
| Starting _
| After_hello _ -> None
| Connected { session_id; _ } -> Some session_id
