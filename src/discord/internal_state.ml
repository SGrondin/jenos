open! Core_kernel

type heartbeat = {
  ms: int;
  mutable until: bool;
  mutable seq: int option;
  mutable ack: int;
  mutable count: int;
}
[@@deriving sexp]

let rec loop heartbeat respond close =
  let%lwt () = Lwt_unix.sleep (heartbeat.ms // 1000) in
  if heartbeat.until then Lwt.return_unit else begin
    Lwt.async (fun () ->
      Lwt.catch (fun () ->
        if heartbeat.ack < heartbeat.count
        then close heartbeat.ack heartbeat.count
        else begin
          heartbeat.count <- heartbeat.count + 1;
          respond @@ Commands.Heartbeat.to_message heartbeat.seq
        end
      ) (fun exn -> Lwt_io.eprintlf "❌ Background loop exn: %s. Please report this bug."
          (Exn.to_string exn))
    );
    loop heartbeat respond close
  end

let start_heartbeat ~heartbeat_interval ~seq respond close =
  let heartbeat = {
    ms = heartbeat_interval;
    until = false;
    seq;
    ack = 0;
    count = 0;
  }
  in
  Lwt.async (fun () -> loop heartbeat respond close);
  heartbeat

let stop_heartbeat heartbeat = heartbeat.until <- true

type t =
| Starting of int option
| After_hello of heartbeat
| Connected of {
    heartbeat: heartbeat;
    session_id: string;
  }
[@@deriving sexp]

let initial () = Starting None

let received_hello ~heartbeat_interval respond close = function
| Starting seq ->
  After_hello (start_heartbeat ~heartbeat_interval ~seq respond close)
| After_hello heartbeat ->
  stop_heartbeat heartbeat;
  After_hello (start_heartbeat ~heartbeat_interval ~seq:heartbeat.seq respond close)
| Connected { heartbeat; session_id } ->
  stop_heartbeat heartbeat;
  Connected {
    heartbeat = start_heartbeat ~heartbeat_interval ~seq:heartbeat.seq respond close;
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

let received_seq seq = function
| Starting _ -> ()
| After_hello heartbeat
| Connected { heartbeat; _ } ->
  Option.iter seq ~f:(fun _s -> heartbeat.seq <- seq)

let received_ack = function
| Starting _ -> ()
| After_hello heartbeat
| Connected { heartbeat; _ } ->
  heartbeat.ack <- heartbeat.ack + 1

let seq = function
| Starting seq
| After_hello { seq; _ }
| Connected { heartbeat = { seq; _ }; _ } -> seq

let terminate = function
| Starting _ -> ()
| After_hello heartbeat
| Connected { heartbeat; _ } -> stop_heartbeat heartbeat

let session_id = function
| Starting _
| After_hello _ -> None
| Connected { session_id; _ } -> Some session_id
