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

let () =
  let default_filename = "config.json" in
  try
    Lwt_main.run
      (match Sys.get_argv () with
      | [| _; "-h" |]
       |[| _; "-help" |]
       |[| _; "--help" |] ->
        let%lwt _config = get_print_config default_filename in
        Lwt.return_unit
      | [| _; "-h"; filename |]
       |[| _; "-help"; filename |]
       |[| _; "--help"; filename |] ->
        let%lwt _config = get_print_config filename in
        Lwt.return_unit
      | args ->
        let filename = if Array.length args >= 2 then args.(1) else default_filename in
        let%lwt config = get_print_config filename in
        Discord.Login.(
          create ~token:config.token
            ~intents:[ GUILDS; GUILD_VOICE_STATES; GUILD_MESSAGES ]
            ?activity:config.status ())
        |> Jenos.create_bot config)
  with
  | Exit -> print_endline "Exit"
  | Unix.Unix_error (e, c, s) ->
    print_endline (sprintf "%s. At: %s(%s)" (Unix.Error.message e) c s);
    exit 1
