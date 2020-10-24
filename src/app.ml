open! Core

let () = Lwt.async_exception_hook := (fun ex ->
    let open Lwt in
    let _ : unit Lwt.t = Lwt_io.eprintlf "💀 UNCAUGHT EXCEPTION: %s" (Exn.to_string ex)
      >>= (fun () -> exit 2)
    in
    ()
  )

let get_print_config filename =
  let%lwt config = Lwt_io.with_file ~flags:Unix.[O_RDONLY; O_NONBLOCK] ~mode:Input filename (fun ic ->
      let%lwt str = Lwt_io.read ic in
      Yojson.Safe.from_string str |> Jenos.config_of_yojson_exn |> Lwt.return
    )
  in
  let%lwt () = Lwt_io.printl (Jenos.sexp_of_config config |> Sexp.to_string_hum) in
  Lwt.return config

let () =
  let default_filename = "config.json" in
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
          else default_filename
        in
        let%lwt config = get_print_config filename in
        Discord.Config.(create ~token:config.token ~intents:[GUILDS; GUILD_VOICE_STATES] ())
        |> Jenos.create_bot config
      end
    )
  with
  | Exit -> print_endline "Exit"
  | Unix.Unix_error (e, c, s) ->
    print_endline (sprintf "%s. At: %s(%s)" (Unix.Error.message e) c s);
    exit 1
