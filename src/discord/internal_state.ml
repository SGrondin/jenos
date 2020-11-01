open! Core_kernel

type heartbeat = {
  ms: int;
  mutable until: bool;
  mutable seq: int option;
  mutable ack: int option;
}
[@@deriving sexp]

let rec loop heartbeat respond =
  let%lwt () = Lwt_unix.sleep (heartbeat.ms // 1000) in
  if heartbeat.until then Lwt.return_unit else begin
    Lwt.async (fun () ->
      Lwt.catch (fun () ->
        let%lwt () = Lwt_io.printlf "HB: %s" (Option.value_map ~default:"!!!" ~f:Int.to_string heartbeat.seq) in
        respond @@ Commands.Heartbeat.to_message heartbeat.seq
      ) (fun exn -> Lwt_io.eprintlf "❌ Background loop exn: %s. Please report this bug."
          (Exn.to_string exn))
    );
    loop heartbeat respond
  end

let start_heartbeat ~heartbeat_interval ~seq respond =
  let heartbeat = {
    ms = heartbeat_interval;
    until = false;
    seq;
    ack = None;
  }
  in
  Lwt.async (fun () -> loop heartbeat respond);
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

let received_seq seq = function
| Starting _ -> ()
| After_hello heartbeat
| Connected { heartbeat; _ } ->
  Option.iter seq ~f:(fun _s -> heartbeat.seq <- seq)

let received_ack = function
| Starting _ -> ()
| After_hello heartbeat
| Connected { heartbeat; _ } ->
  heartbeat.ack <- heartbeat.seq

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
