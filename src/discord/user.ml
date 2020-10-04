open! Core_kernel

let (=) = Poly.(=)
type t = {
  id: string;
  username: string;
  discriminator: string;
  avatar: string option;
  bot: bool option [@default None];
  system: bool option [@default None];
  mfa_enabled: bool option [@default None];
  locale: string option [@default None];
  verified: bool option [@default None];
  email: string option [@default None];
  flags: int option [@default None];
  premium_type: int option [@default None];
  public_flags: int option [@default None];
}
[@@deriving sexp, yojson { exn = true; strict = false }]
let (=) = Core_kernel.(=)
