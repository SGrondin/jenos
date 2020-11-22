open! Core_kernel

type heartbeat = {
  mutable until: bool;
  mutable seq: int option;
  mutable ack: int;
  mutable count: int;
}
[@@deriving sexp]

type heartbeat_loop = {
  on_exn: exn -> unit Lwt.t;
  interval: int;
  respond: Message.Send.t -> unit Lwt.t;
  close: ack:int -> count:int -> unit Lwt.t;
}

let rec loop heartbeat ({ on_exn; respond; close; interval } as heartbeat_loop) =
  let%lwt () = Lwt_unix.sleep (interval // 1000) in
  if heartbeat.until then Lwt.return_unit else begin
    let%lwt () =
      Lwt.catch (fun () ->
        if heartbeat.ack < heartbeat.count
        then close ~ack:heartbeat.ack ~count:heartbeat.count
        else begin
          heartbeat.count <- heartbeat.count + 1;
          respond @@ Commands.Heartbeat.to_message heartbeat.seq
        end
      ) on_exn
    in
    loop heartbeat heartbeat_loop
  end

let start_heartbeat heartbeat_loop ~seq =
  let heartbeat = {
    until = false;
    seq;
    ack = 0;
    count = 0;
  }
  in
  Lwt.async (fun () -> loop heartbeat heartbeat_loop);
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

let create () = Starting None

let received_hello heartbeat_loop = function
| Starting seq ->
  After_hello (start_heartbeat heartbeat_loop ~seq)
| After_hello heartbeat ->
  stop_heartbeat heartbeat;
  After_hello (start_heartbeat heartbeat_loop ~seq:heartbeat.seq)
| Connected { heartbeat; session_id } ->
  stop_heartbeat heartbeat;
  Connected {
    heartbeat = start_heartbeat heartbeat_loop ~seq:heartbeat.seq;
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

let terminate = function
| Starting _ -> ()
| After_hello heartbeat
| Connected { heartbeat; _ } -> stop_heartbeat heartbeat

let seq = function
| Starting seq
| After_hello { seq; _ }
| Connected { heartbeat = { seq; _ }; _ } -> seq

let session_id = function
| Starting _
| After_hello _ -> None
| Connected { session_id; _ } -> Some session_id
