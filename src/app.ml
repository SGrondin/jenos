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

let rec main state =
  let%lwt res = Discord.Gateway.get () in
  let%lwt () = [%sexp_of: Discord.Gateway.response] res |> Sexp.to_string |> Lwt_io.printl in
  Lwt.catch (fun () -> Discord.Ws.connect state res.url)
    (function
    | Discord.State.Resume state ->
      let%lwt () = sleep_random () in
      main state
    | Discord.State.Reconnect ->
      let%lwt () = sleep_random () in
      main (Discord.State.initial ())
    | exn ->
      let%lwt () = Lwt_io.eprintlf "❌ Error in main loop: %s" (Exn.to_string exn) in
      let%lwt () = sleep_random () in
      main (Discord.State.initial ())
    )

let () =
  begin match Sys.get_argv () with
  | [| _; "-h" |]
  | [| _; "--help" |] -> ()
  | _ ->
    try Lwt_main.run @@ main (Discord.State.initial ())
    with Exit -> print_endline "Exit"
  end
