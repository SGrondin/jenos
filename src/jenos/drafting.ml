open! Core_kernel
open Lwt.Syntax
open Config

module Make (M : sig
  type t [@@deriving sexp, compare, enumerate]
end) =
struct
  type t = M.t [@@deriving sexp, compare, enumerate]

  module Set = Set.Make (M)

  let random set =
    Set.length set |> Random.int |> Set.nth set |> Option.value_exn ~message:"Random: Empty set!"

  let set = Set.of_list M.all

  let __t_of_sexp__ = M.t_of_sexp
end

module Flank = Make (struct
  type t =
    [ `Androxus
    | `Buck
    | `Evie
    | `Koga
    | `Lex
    | `Maeve
    | `Moji
    | `Skye
    | `Talus
    | `Vatu
    | `Vora
    | `Zhin
    ]
  [@@deriving sexp, compare, enumerate]
end)

module Frontline = Make (struct
  type t =
    [ `Ash
    | `Atlas
    | `Barik
    | `Fernando
    | `Inara
    | `Khan
    | `Makoa
    | `Raum
    | `Ruckus
    | `Terminus
    | `Torvalds (* | `Yagorath *)
    ]
  [@@deriving sexp, compare, enumerate]
end)

module Damage = Make (struct
  type t =
    [ `BombKing
    | `Cassie
    | `Dredge
    | `Drogoz
    | `Imani
    | `Kinessa
    | `Lian
    | `Octavia
    | `Shalin
    | `Strix
    | `Tiberius
    | `Tyra
    | `Viktor
    | `Vivian
    | `Willo
    ]
  [@@deriving sexp, compare, enumerate]
end)

module Support = Make (struct
  type t =
    [ `Corvus
    | `Furia
    | `Grohk
    | `Grover
    | `Io
    | `Jenos
    | `Maldamba
    | `Pip
    | `Seris
    | `Ying
    ]
  [@@deriving sexp, compare, enumerate]
end)

module Champion = Make (struct
  type t =
    [ Flank.t
    | Frontline.t
    | Damage.t
    | Support.t
    ]
  [@@deriving sexp, compare, enumerate]
end)

type state = {
  flanks: Flank.Set.t;
  frontlines: Frontline.Set.t;
  damages: Damage.Set.t;
  supports: Support.Set.t;
  champions: Champion.Set.t;
  selected: Champion.Set.t;
}

type kind =
  | Flank
  | Frontline
  | Damage
  | Support
  | Any

(* let () = Random.self_init () *)

let draft_n n =
  let draft state kind =
    let pick, state =
      (* Invariant: "Any" picks must be at the end *)
      match kind with
      | Flank ->
        let pick = Flank.random state.flanks in
        (pick :> Champion.t), { state with flanks = Flank.Set.remove state.flanks pick }
      | Frontline ->
        let pick = Frontline.random state.frontlines in
        (pick :> Champion.t), { state with frontlines = Frontline.Set.remove state.frontlines pick }
      | Damage ->
        let pick = Damage.random state.damages in
        (pick :> Champion.t), { state with damages = Damage.Set.remove state.damages pick }
      | Support ->
        let pick = Support.random state.supports in
        (pick :> Champion.t), { state with supports = Support.Set.remove state.supports pick }
      | Any -> Champion.random state.champions, state
    in
    {
      state with
      selected = Champion.Set.add state.selected pick;
      champions = Champion.Set.remove state.champions pick;
    }
  in
  Array.create ~len:(n - 5) Any
  |> Array.append [| Support; Support; Support; Frontline; Frontline |]
  |> Array.fold ~f:draft
       ~init:
         {
           flanks = Flank.set;
           frontlines = Frontline.set;
           damages = Damage.set;
           supports = Support.set;
           champions = Champion.set;
           selected = Champion.Set.empty;
         }

let on_message_create { token; drafting; _ } = function
| Data.Message.
    {
      type_ = DEFAULT;
      content = "Draft 10!";
      id = message_id;
      guild_id;
      channel_id;
      author = { id = user_id; _ };
      _;
    }
  when [%equal: Basics.Snowflake.t] user_id drafting.allowed_user_id ->
  let message_reference : Data.Message.Reference.t =
    {
      message_id = Some message_id;
      channel_id = Some channel_id;
      guild_id;
      fail_if_not_exists = Some true;
    }
  in
  let { selected; _ } = draft_n 10 in
  let content =
    Champion.Set.to_array selected
    |> Array.map ~f:(sprintf !"%{sexp: Champion.t}")
    |> Array.sorted_copy ~compare:[%compare: string]
    |> Array.mapi ~f:(fun i -> sprintf "%d. %s" (i + 1))
    |> String.concat_array ~sep:"\n"
  in
  let+ _msg = Rest.Channel.create_message ~token ~channel_id ~content ~message_reference () in
  ()
| _ -> Lwt.return_unit
