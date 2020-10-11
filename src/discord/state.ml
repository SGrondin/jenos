open! Core_kernel

type heartbeat = {
  ms: int;
  until: bool ref;
  seq: int option ref;
}
[@@deriving sexp]

let rec loop sec until seq send =
  let%lwt () = Lwt_unix.sleep sec in
  if !until then Lwt.return_unit else begin
    Lwt.async (fun () ->
      Lwt.catch (fun () ->
        Commands.Heartbeat.to_message !seq
        |> Message.respond send
      ) (fun exn -> Lwt_io.eprintlf "Background loop exn: %s" (Exn.to_string exn))
    );
    loop sec until seq send
  end

let start_heartbeat ~heartbeat_interval ~seq send =
  let { ms; until; seq } as heartbeat = {
    ms = heartbeat_interval;
    until = ref false;
    seq;
  }
  in
  Lwt.async (fun () -> loop (ms // 1000) until seq send);
  heartbeat

let stop_heartbeat { until; _ } = until := true

type t =
| Starting of int option ref
| After_hello of heartbeat
| Connected of {
    heartbeat: heartbeat;
    session_id: string;
    tracker: Events.Voice_state_update.t String.Map.t;
  }
[@@deriving sexp]

let initial () = Starting (ref None)

exception Resume of t
exception Reconnect

let received_hello ~heartbeat_interval send = function
| Starting seq ->
  After_hello (start_heartbeat ~heartbeat_interval ~seq send)
| After_hello heartbeat ->
  stop_heartbeat heartbeat;
  After_hello (start_heartbeat ~heartbeat_interval ~seq:heartbeat.seq send)
| Connected { heartbeat; session_id; tracker } ->
  stop_heartbeat heartbeat;
  Connected {
    heartbeat = start_heartbeat ~heartbeat_interval ~seq:heartbeat.seq send;
    session_id;
    tracker;
  }

let received_ready ~session_id = function
| After_hello heartbeat -> Connected {
    heartbeat;
    session_id;
    tracker = String.Map.empty;
  }
| (Starting _ as x)
| (Connected _ as x) ->
  failwithf "Invalid state transition to_connected: %s" (sexp_of_t x |> Sexp.to_string) ()

let track_user ~key ~data = function
| Connected ({ tracker; _ } as inner) ->
  Connected { inner with tracker = String.Map.set ~key ~data tracker }
| (Starting _ as x)
| (After_hello _ as x) ->
  failwithf "Invalid state transition set: %s" (sexp_of_t x |> Sexp.to_string) ()

let forget_user ~key = function
| Connected ({ tracker; _ } as inner) ->
  Connected { inner with tracker = String.Map.remove tracker key }
| (Starting _ as x)
| (After_hello _ as x) ->
  failwithf "Invalid state transition set: %s" (sexp_of_t x |> Sexp.to_string) ()

let size = function
| Connected { tracker; _ } -> String.Map.length tracker
| (Starting _ as x)
| (After_hello _ as x) ->
  failwithf "Invalid state transition size: %s" (sexp_of_t x |> Sexp.to_string) ()

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
