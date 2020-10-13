open! Core_kernel
open Websocket
open Websocket_lwt_unix

let shutdown = ref None
let make_shutdown send () =
  Lwt_main.at_exit (fun () ->
    Lwt_unix.with_timeout 1.0 (fun () -> send @@ Frame.close 1000)
  );
  raise Exit

let _sigint = Lwt_unix.on_signal Sys.sigint (fun _ -> Option.call () ~f:!shutdown)
let _sigterm = Lwt_unix.on_signal Sys.sigterm (fun _ -> Option.call () ~f:!shutdown)

let handle send state = function
| Frame.{ opcode = Ping; _ } ->
  let%lwt () = send @@ Frame.create ~opcode:Frame.Opcode.Pong () in
  Lwt.return state

| Frame.{ opcode = Close; _ } ->
  let%lwt () = send @@ Frame.close 1000 in
  raise (State.Resume state)

| Frame.{ opcode = Pong; _ } ->
  Lwt.return state

| Frame.{ opcode = Text; content; _ }
| Frame.{ opcode = Binary; content; _ } ->
  let%lwt () = Lwt_io.printlf "<<< %s" content in
  Message.of_string (State.seq state) content
  |> Router.handle send state

| frame ->
  let%lwt () = send @@ Frame.close 0 in
  failwithf "Unhandled frame: %s" (Frame.show frame) ()

let connect state uri : unit Lwt.t =
  let uri =
    begin match Uri.scheme uri with
    | None
    | Some "wss" -> Uri.with_scheme uri (Some "https")
    | Some "ws" -> Uri.with_scheme uri (Some "http")
    | Some x -> failwithf "Invalid scheme in WS connect: %s" x ()
    end
    |> Fn.flip Uri.add_query_params ["v", ["8"]; "encoding", ["json"]]
  in

  let%lwt recv, send =
    let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
    let ctx = Conduit_lwt_unix.default_ctx in
    let%lwt client = Conduit_lwt_unix.endp_to_client ~ctx endp in
    with_connection ~ctx client uri
  in

  shutdown := Some (make_shutdown send);

  let rec loop state =
    let%lwt updated = Lwt.catch (fun () ->
        let%lwt frame = recv () in
        handle send state frame
      ) (fun exn ->
        Option.iter (State.heartbeat state) ~f:State.stop_heartbeat;
        let%lwt () = Lwt.pick [
            Lwt_unix.sleep 1.0;
            send @@ Frame.close 1001;
          ]
        in
        begin match exn with
        | End_of_file -> raise (State.Resume state)
        | exn -> raise exn
        end
      )
    in
    loop updated
  in
  loop state
