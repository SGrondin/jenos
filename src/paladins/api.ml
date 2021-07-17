open! Core_kernel

module Make (M : Call.S) = struct
  module Session = Session.Make (M)
  module Player = Player.Make (M)
  module Match = Match.Make (M)
end
