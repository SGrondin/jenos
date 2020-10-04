open! Core_kernel
open Websocket
open Websocket_lwt_unix

let connect uri : unit Lwt.t =
  let uri =
    begin match Uri.scheme uri with
    | Some "ws" -> Uri.with_scheme uri (Some "http")
    | Some "wss" -> Uri.with_scheme uri (Some "https")
    | scheme -> failwithf "Invalid scheme in WS connect: %s" (Option.value ~default:"" scheme) ()
    end
    |> (fun u -> Uri.add_query_params u ["v", ["8"]; "encoding", ["json"]])
  in

  let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
  let%lwt recv, send =
    let open Conduit_lwt_unix in
    let%lwt client = endp_to_client ~ctx:default_ctx endp in
    with_connection ~ctx:default_ctx client uri
  in

  let react (fr : Frame.t) =
    begin match fr.opcode with
    | Frame.Opcode.Ping ->
      print_endline "!!! PING";
      send @@ Frame.create ~opcode:Frame.Opcode.Pong ()

    | Frame.Opcode.Close ->
      let%lwt () = send @@ Frame.close 1000 in
      Lwt.fail Exit

    | Frame.Opcode.Pong ->
      print_endline "!!! PONG";
      Lwt.return_unit

    | Frame.Opcode.Text
    | Frame.Opcode.Binary ->
      let%lwt () = Lwt_io.printlf "<<< %s <<<" fr.content in
      let message = Message.of_string fr.content in
      Router.handle send message

    | _ ->
      let%lwt () = send @@ Frame.close 1002 in
      raise Exit
    end
  in
  let rec react_forever () =
    let open Lwt.Infix in
    recv () >>= react >>= react_forever
  in
  react_forever ()
