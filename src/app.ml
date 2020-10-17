open! Core

let () = Lwt.async_exception_hook := (fun ex ->
    let open Lwt in
    let _ : unit Lwt.t = Lwt_io.eprintlf "💀 UNCAUGHT EXCEPTION: %s" (Exn.to_string ex)
      >>= (fun () -> exit 2)
    in
    ()
  )

let sleep_random () =
  let%lwt () = Lwt_io.printl "⏳ Reconnecting soon" in
  let%lwt () = Random.float_range 2.0 5.0 |> Lwt_unix.sleep in
  Lwt_io.printl "🌐 Reconnecting..."

let rec event_loop (config : Discord.Config.t) state =
  let%lwt res = Discord.Gateway.get ~token:config.token in
  let%lwt () = [%sexp_of: Discord.Gateway.response] res |> Sexp.to_string |> Lwt_io.printl in
  Lwt.catch (fun () -> Discord.Ws.connect config state res.url)
    (function
    | Discord.State.Resume state ->
      let%lwt () = sleep_random () in
      event_loop config state
    | Discord.State.Reconnect ->
      let%lwt () = sleep_random () in
      event_loop config (Discord.State.initial ())
    | exn ->
      let%lwt () = Lwt_io.eprintlf "❌ Error in event_loop: %s" (Exn.to_string exn) in
      let%lwt () = sleep_random () in
      event_loop config (Discord.State.initial ())
    )

let get_print_config filename =
  let%lwt config = Lwt_io.with_file ~flags:Unix.[O_RDONLY; O_NONBLOCK] ~mode:Input filename (fun ic ->
      let%lwt str = Lwt_io.read ic in
      Sexp.of_string_conv_exn str Discord.Config.t_of_sexp
      |> Lwt.return
    )
  in
  let%lwt () = Lwt_io.printl (Discord.Config.sexp_of_t config |> Sexp.to_string_hum) in
  Lwt.return config

let () =
  let default_filename = "config" in
  try Lwt_main.run (
      begin match Sys.get_argv () with
      | [| _; "-h" |] | [| _; "-help" |] | [| _; "--help" |] ->
        let%lwt _config = get_print_config default_filename in
        Lwt.return_unit
      | [| _; "-h"; filename |] | [| _; "-help"; filename |] | [| _; "--help"; filename |] ->
        let%lwt _config = get_print_config filename in
        Lwt.return_unit
      | args ->
        let filename =
          if Array.length args >= 2
          then Array.get args 1
          else "config"
        in
        let%lwt config = get_print_config filename in
        event_loop config (Discord.State.initial ())
      end
    )
  with
  | Exit -> print_endline "Exit"
  | Unix.Unix_error (e, c, s) ->
    print_endline (sprintf "%s. At: %s(%s)" (Unix.Error.message e) c s);
    exit 1
