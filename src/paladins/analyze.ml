open! Core_kernel

type measurement = {
  total: int64;
  n: int64;
}

type stats = {
  match_duration: measurement;
  match_duration_full: measurement;
}

let measure ~f arr =
  {
    total = Array.fold arr ~init:0L ~f:Int64.((fun acc x -> f x + acc));
    n = Array.length arr |> Int.to_int64;
  }

let matches_to_stats (games : Match.Match_player.player_x_match array) =
  {
    match_duration =
      measure games ~f:(fun Match.Match_player.{ match_duration; _ } ->
          Types.Seconds.to_int64 match_duration);
    match_duration_full =
      measure games ~f:(fun Match.Match_player.{ match_duration_full; _ } ->
          Types.Seconds.to_int64 match_duration_full);
  }

let analyze_players (games : Match.Match_player.t array) =
  Array.fold games ~init:Types.Player_id.Map.empty ~f:(fun acc game ->
      List.fold game.entries ~init:acc ~f:(fun acc entry ->
          Types.Player_id.Map.update acc entry.account.id ~f:(function
            | None -> [ entry ]
            | Some ll -> entry :: ll)))
  |> Types.Player_id.Map.map ~f:Array.of_list_rev
