open! Core_kernel

let get name =
  Sys.getenv_opt name
  |> Option.value_exn ~message:(sprintf "Missing env var: %s" name) ~here:[%here]
