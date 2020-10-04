open! Core

let () = Lwt.async_exception_hook := (fun ex ->
    let open Lwt in
    let _ : unit Lwt.t = Lwt_io.eprintlf "💀 UNCAUGHT EXCEPTION: %s" (Exn.to_string ex)
      >>= (fun () -> exit 2)
    in
    ()
  )

let main () =
  let%lwt res = Discord.Gateway.get () in
  let%lwt () =
    [%sexp_of: Discord.Gateway.response] res
    |> Sexp.to_string_hum
    |> Lwt_io.printl
  in
  let rec run url =
    Lwt.catch (fun () -> Discord.Ws.connect url)
      (function
      | Discord.Router.Reconnect -> run url
      | exn -> raise exn
      )
  in
  run res.url

let () =
  Lwt_main.run @@ main ()
