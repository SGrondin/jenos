open! Core_kernel

type t = {
  id: string;
  username: string;
  discriminator: string;
  avatar: string option;
  bot: bool option;
  system: bool option;
  mfa_enabled: bool option;
  locale: string option;
  verified: bool option;
  email: string option;
  flags: int option;
  premium_type: int option;
  public_flags: int option;
}
[@@deriving sexp, compare, fields, yojson]
