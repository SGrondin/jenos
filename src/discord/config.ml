open! Core_kernel

type t = {
  token: string;
  status: Commands.Identify.activity option [@default None];
  role: string;
  line2: string;
  line4: string;
  vc_channel: string;
  text_channel: string;
} [@@deriving sexp]
