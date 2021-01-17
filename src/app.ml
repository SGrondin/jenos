open! Core

let () =
  Lwt.async_exception_hook :=
    fun ex ->
      let open Lwt in
      let (_ : unit Lwt.t) =
        Lwt_io.printlf "💀 UNCAUGHT EXCEPTION: %s" (Exn.to_string ex) >>= fun () -> exit 2
      in
      ()

let () =
  Lwt_engine.set ~transfer:true ~destroy:true
    ((* Linux *)
    try new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.epoll () with
    | _ ->
      (* MacOS *)
      new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.kqueue ())

let shutdown = ref None

let () =
  let handler signal =
    let caml = Signal.of_caml_int signal in
    print_endline (sprintf "Received %s (%i)" (Signal.to_string caml) (Signal.to_system_int caml));
    Option.call () ~f:!shutdown
  in
  let _sigint = Lwt_unix.on_signal Signal.(to_caml_int int) handler in
  let _sigterm = Lwt_unix.on_signal Signal.(to_caml_int term) handler in
  let _sigquit = Lwt_unix.on_signal Signal.(to_caml_int quit) handler in
  let _sighup = Lwt_unix.on_signal Signal.(to_caml_int hup) handler in
  let _sigabort = Lwt_unix.on_signal Signal.(to_caml_int abrt) handler in
  ()

let get_print_config filename =
  let open Jenos__ in
  let%lwt config =
    Lwt_io.with_file
      ~flags:Unix.[ O_RDONLY; O_NONBLOCK ]
      ~mode:Input filename
      (fun ic ->
        let%lwt str = Lwt_io.read ic in
        Yojson.Safe.from_string str |> Config.of_yojson |> Result.ok_or_failwith |> Lwt.return)
  in
  let%lwt () = Lwt_io.printl (Config.sexp_of_t config |> Sexp.to_string_hum) in
  Lwt.return config

let run_app p () =
  try Lwt_main.run (p ()) with
  | Exit -> print_endline "Exit"
  | Unix.Unix_error (e, c, s) ->
    print_endline (sprintf "%s. At: %s(%s)" (Unix.Error.message e) c s);
    exit 1

let script =
  let open Command in
  let task () =
    let open Discord.Data in
    let _ss = Basics.Snowflake.to_string in
    let _sos = Basics.Snowflake.of_string in
    Lwt.return_unit
  in
  basic ~summary:"" (Param.return (run_app task))

let filename_param =
  let open Command.Param in
  anon (maybe_with_default "config.json" ("filename" %: string))

let debug =
  let open Command in
  let open Let_syntax in
  let task filename () =
    let%lwt _config = get_print_config filename in
    Lwt.return_unit
  in
  let param =
    let%map_open filename = filename_param in
    run_app (task filename)
  in
  basic ~summary:"" param

let bot =
  let open Command in
  let open Let_syntax in
  let task filename () =
    let%lwt ({ token; activity_type; activity_name; _ } as config) = get_print_config filename in
    let login =
      Discord.Login.(
        create ~token
          ~intents:[ GUILDS; GUILD_VOICE_STATES; GUILD_MESSAGES ]
          ?activity_name ?activity_type ())
    in
    let p, stop = Jenos.create_bot config login in
    shutdown := Some stop;
    p
  in
  let param =
    let%map_open filename = filename_param in
    run_app (task filename)
  in
  basic ~summary:"" param

let () = Command.run @@ Command.group ~summary:"" [ "script", script; "debug", debug; "bot", bot ]
