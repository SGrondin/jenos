open! Core_kernel

module Match_player = struct
  module Raw = struct
    type t = {
      account_level: int; [@key "Account_Level"]
      active_player_id: Types.Player_id.t; [@key "ActivePlayerId"]
      assists: int; [@key "Assists"]
      ban_id_1: Types.IntOpt.t; [@key "BanId1"]
      ban_id_2: Types.IntOpt.t; [@key "BanId2"]
      ban_id_3: Types.IntOpt.t; [@key "BanId3"]
      ban_id_4: Types.IntOpt.t; [@key "BanId4"]
      ban_1: string option; [@key "Ban_1"]
      ban_2: string option; [@key "Ban_2"]
      ban_3: string option; [@key "Ban_3"]
      ban_4: string option; [@key "Ban_4"]
      deprecated_camps_cleared: int; [@key "Camps_Cleared"]
      champion_id: int; [@key "ChampionId"]
      damage_bot: int; [@key "Damage_Bot"]
      damage_done_in_hand: int; [@key "Damage_Done_In_Hand"]
      damage_done_magical: int; [@key "Damage_Done_Magical"]
      damage_done_physical: int; [@key "Damage_Done_Physical"]
      damage_mitigated: int; [@key "Damage_Mitigated"]
      damage_player: int; [@key "Damage_Player"]
      damage_taken: int; [@key "Damage_Taken"]
      damage_taken_magical: int; [@key "Damage_Taken_Magical"]
      damage_taken_physical: int; [@key "Damage_Taken_Physical"]
      deaths: int; [@key "Deaths"]
      deprecated_distance_traveled: int; [@key "Distance_Traveled"]
      entry_datetime: Types.DateTime.t; [@key "Entry_Datetime"]
      deprecated_final_match_level: int; [@key "Final_Match_Level"]
      gold_earned: int; [@key "Gold_Earned"]
      deprecated_gold_per_minute: int; [@key "Gold_Per_Minute"]
      healing: int; [@key "Healing"]
      healing_bot: int; [@key "Healing_Bot"]
      healing_player_self: int; [@key "Healing_Player_Self"]
      talent: string; [@key "Item_Purch_6"]
      loadout_id_1: int; [@key "ItemId1"]
      loadout_id_2: int; [@key "ItemId2"]
      loadout_id_3: int; [@key "ItemId3"]
      loadout_id_4: int; [@key "ItemId4"]
      loadout_id_5: int; [@key "ItemId5"]
      loadout_id_6: int; [@key "ItemId6"]
      loadout_1: string; [@key "Item_Purch_1"]
      loadout_2: string; [@key "Item_Purch_2"]
      loadout_3: string; [@key "Item_Purch_3"]
      loadout_4: string; [@key "Item_Purch_4"]
      loadout_5: string; [@key "Item_Purch_5"]
      loadout_level_1: Types.IntOpt.t; [@key "ItemLevel1"]
      loadout_level_2: Types.IntOpt.t; [@key "ItemLevel2"]
      loadout_level_3: Types.IntOpt.t; [@key "ItemLevel3"]
      loadout_level_4: Types.IntOpt.t; [@key "ItemLevel4"]
      loadout_level_5: Types.IntOpt.t; [@key "ItemLevel5"]
      loadout_level_6: Types.IntOpt.t; [@key "ItemLevel6"]
      item_id_1: Types.IntOpt.t; [@key "ActiveId1"]
      item_id_2: Types.IntOpt.t; [@key "ActiveId2"]
      item_id_3: Types.IntOpt.t; [@key "ActiveId3"]
      item_id_4: Types.IntOpt.t; [@key "ActiveId4"]
      item_1: Types.StrOpt.t; [@key "Item_Active_1"]
      item_2: Types.StrOpt.t; [@key "Item_Active_2"]
      item_3: Types.StrOpt.t; [@key "Item_Active_3"]
      item_4: Types.StrOpt.t; [@key "Item_Active_4"]
      item_level_1: Types.IntOpt.t; [@key "ActiveLevel1"]
      item_level_2: Types.IntOpt.t; [@key "ActiveLevel2"]
      item_level_3: Types.IntOpt.t; [@key "ActiveLevel3"]
      item_level_4: Types.IntOpt.t; [@key "ActiveLevel4"]
      killing_spree: int; [@key "Killing_Spree"]
      kills_bot: int; [@key "Kills_Bot"]
      deprecated_kills_double: int; [@key "Kills_Double"]
      deprecated_kills_fire_giant: int; [@key "Kills_Fire_Giant"]
      deprecated_kills_first_blood: int; [@key "Kills_First_Blood"]
      deprecated_kills_gold_fury: int; [@key "Kills_Gold_Fury"]
      deprecated_kills_penta: int; [@key "Kills_Penta"]
      deprecated_kills_phoenix: int; [@key "Kills_Phoenix"]
      kills_player: int; [@key "Kills_Player"]
      deprecated_kills_quadra: int; [@key "Kills_Quadra"]
      deprecated_kills_siege_juggernaut: int; [@key "Kills_Siege_Juggernaut"]
      deprecated_kills_single: int; [@key "Kills_Single"]
      deprecated_kills_triple: int; [@key "Kills_Triple"]
      deprecated_kills_wild_juggernaut: int; [@key "Kills_Wild_Juggernaut"]
      league_losses: int; [@key "League_Losses"]
      league_points: int; [@key "League_Points"]
      league_tier: int; [@key "League_Tier"]
      league_wins: int; [@key "League_Wins"]
      map_game: string; [@key "Map_Game"]
      mastery_level: int; [@key "Mastery_Level"]
      match_id: Types.Match_id.t; [@key "Match"]
      match_duration: Types.Seconds.t; [@key "Match_Duration"]
      merged_players: int option; [@key "MergedPlayers"]
      minutes: int; [@key "Minutes"]
      multi_kill_max: int; [@key "Multi_kill_Max"]
      objective_assists: int; [@key "Objective_Assists"]
      party_id: int; [@key "PartyId"]
      platform: string; [@key "Platform"]
      rank_stat_league: Types.IntOpt.t; [@key "Rank_Stat_League"]
      champion_name: string; [@key "Reference_Name"]
      region: string; [@key "Region"]
      skin: string; [@key "Skin"]
      skin_id: int; [@key "SkinId"]
      structure_damage: int; [@key "Structure_Damage"]
      deprecated_surrendered: int; [@key "Surrendered"]
      taskforce: int; [@key "TaskForce"]
      team1_score: int; [@key "Team1Score"]
      team2_score: int; [@key "Team2Score"]
      deprecated_team_id: Types.IntOpt.t; [@key "TeamId"]
      team_name: Types.StrOpt.t; [@key "Team_Name"]
      time_in_match_seconds: Types.Seconds.t; [@key "Time_In_Match_Seconds"]
      deprecated_towers_destroyed: int; [@key "Towers_Destroyed"]
      deprecated_wards_placed: int; [@key "Wards_Placed"]
      win_status: Win_status.t; [@key "Win_Status"]
      winning_taskforce: int; [@key "Winning_TaskForce"]
      has_replay: Types.Boolean.t; [@key "hasReplay"]
      hz_gamer_tag: string option; [@key "hz_gamer_tag"]
      hz_player_name: string option; [@key "hz_player_name"]
      match_queue_id: Mode.t; [@key "match_queue_id"]
      mode_name: string; [@key "name"]
      player_id: Types.Player_id.t; [@key "playerId"]
      player_name: string; [@key "playerName"]
      player_portal_id: Types.IntStr.t; [@key "playerPortalId"]
      player_portal_user_id: Types.IntStr.t; [@key "playerPortalUserId"]
      ret_msg: string option; [@default None]
    }
    [@@deriving sexp, compare, equal, yojson { strict = false }]
  end

  type account = {
    id: Types.Player_id.t;
    name: string;
    hz_gamer_tag: string option;
    hz_player_name: string option;
    level: int;
    taskforce: int;
    party_id: int;
    platform: string;
    rank_stat_league: Types.IntOpt.t;
    region: string;
    player_portal_id: Types.IntStr.t;
    player_portal_user_id: Types.IntStr.t;
  }
  [@@deriving sexp, compare, equal]

  type item = {
    id: int;
    name: string;
    level: int;
  }
  [@@deriving sexp, compare, equal]

  type champion = {
    id: int;
    name: string;
    level: int;
    talent: string;
    skin: string;
    skin_id: int;
    item1: item option;
    item2: item option;
    item3: item option;
    item4: item option;
    loadout1: item;
    loadout2: item;
    loadout3: item;
    loadout4: item;
    loadout5: item;
  }
  [@@deriving sexp, compare, equal]

  type damage = {
    bot: int;
    done_in_hand: int;
    done_magical: int;
    done_physical: int;
    mitigated: int;
    player: int;
    taken: int;
    taken_magical: int;
    taken_physical: int;
  }
  [@@deriving sexp, compare, equal]

  type healing = {
    player: int;
    bot: int;
    self: int;
  }
  [@@deriving sexp, compare, equal]

  type player_stats = {
    damage: damage;
    deaths: int;
    gold_earned: int;
    healing: healing;
    killing_spree: int;
    kills_bot: int;
    kills_player: int;
    league_losses: int;
    league_points: int;
    league_tier: int;
    league_wins: int;
    assists: int;
    multi_kill_max: int;
    objective_assists: int;
    structure_damage: int;
    win_status: Win_status.t;
  }
  [@@deriving sexp, compare, equal]

  type ban = {
    id: int;
    name: string;
  }
  [@@deriving sexp, compare, equal]

  type outcome = {
    team1_score: int;
    team2_score: int;
    team_name: Types.StrOpt.t;
    winning_taskforce: int;
    has_replay: Types.Boolean.t;
  }
  [@@deriving sexp, compare, equal]

  type player_x_match = {
    match_id: Types.Match_id.t;
    datetime: Types.DateTime.t;
    match_mode: Mode.t;
    match_duration: Types.Seconds.t;
    match_duration_full: Types.Seconds.t;
    merged_players: int option;
    map_name: string;
    outcome: outcome;
    account: account;
    champion: champion;
    player_stats: player_stats;
    ban1: ban option;
    ban2: ban option;
    ban3: ban option;
    ban4: ban option;
  }
  [@@deriving sexp, compare, equal]

  let player_x_match_of_yojson json =
    let open Result.Let_syntax in
    let%bind raw = [%of_yojson: Raw.t] json in
    let make_item id name level =
      let open Option.Let_syntax in
      let%map id = id
      and name = name
      and level = level in
      { id; name; level }
    in
    (* TODO: Check for ret_msg *)
    Result.try_with (fun () ->
        {
          match_id = raw.match_id;
          datetime = raw.entry_datetime;
          match_mode = raw.match_queue_id;
          match_duration = raw.match_duration;
          match_duration_full = raw.time_in_match_seconds;
          merged_players = raw.merged_players;
          map_name = raw.map_game;
          outcome =
            {
              team1_score = raw.team1_score;
              team2_score = raw.team2_score;
              team_name = raw.team_name;
              winning_taskforce = raw.winning_taskforce;
              has_replay = raw.has_replay;
            };
          account =
            {
              (* Note: there's also player_id *)
              id = raw.active_player_id;
              name = raw.player_name;
              hz_gamer_tag = raw.hz_gamer_tag;
              hz_player_name = raw.hz_player_name;
              level = raw.account_level;
              taskforce = raw.taskforce;
              party_id = raw.party_id;
              platform = raw.platform;
              (* TODO: Convert to a variant *)
              rank_stat_league = raw.rank_stat_league;
              region = raw.region;
              player_portal_id = raw.player_portal_id;
              player_portal_user_id = raw.player_portal_user_id;
            };
          champion =
            {
              id = raw.champion_id;
              name = raw.champion_name;
              level = raw.mastery_level;
              talent = raw.talent;
              skin = raw.skin;
              skin_id = raw.skin_id;
              item1 = make_item raw.item_id_1 raw.item_1 raw.item_level_1;
              item2 = make_item raw.item_id_2 raw.item_2 raw.item_level_2;
              item3 = make_item raw.item_id_3 raw.item_3 raw.item_level_3;
              item4 = make_item raw.item_id_4 raw.item_4 raw.item_level_4;
              loadout1 =
                Option.map raw.loadout_level_1 ~f:(fun level ->
                    { id = raw.loadout_id_1; name = raw.loadout_1; level })
                |> Option.value_exn ~here:[%here];
              loadout2 =
                Option.map raw.loadout_level_2 ~f:(fun level ->
                    { id = raw.loadout_id_2; name = raw.loadout_2; level })
                |> Option.value_exn ~here:[%here];
              loadout3 =
                Option.map raw.loadout_level_3 ~f:(fun level ->
                    { id = raw.loadout_id_3; name = raw.loadout_3; level })
                |> Option.value_exn ~here:[%here];
              loadout4 =
                Option.map raw.loadout_level_4 ~f:(fun level ->
                    { id = raw.loadout_id_4; name = raw.loadout_4; level })
                |> Option.value_exn ~here:[%here];
              loadout5 =
                Option.map raw.loadout_level_5 ~f:(fun level ->
                    { id = raw.loadout_id_5; name = raw.loadout_5; level })
                |> Option.value_exn ~here:[%here];
            };
          player_stats =
            {
              damage =
                {
                  bot = raw.damage_bot;
                  done_in_hand = raw.damage_done_in_hand;
                  done_magical = raw.damage_done_magical;
                  done_physical = raw.damage_done_physical;
                  mitigated = raw.damage_mitigated;
                  player = raw.damage_player;
                  taken = raw.damage_taken;
                  taken_magical = raw.damage_taken_magical;
                  taken_physical = raw.damage_taken_physical;
                };
              deaths = raw.deaths;
              gold_earned = raw.gold_earned;
              healing = { player = raw.healing; bot = raw.healing_bot; self = raw.healing_player_self };
              killing_spree = raw.killing_spree;
              kills_bot = raw.kills_bot;
              kills_player = raw.kills_player;
              league_losses = raw.league_losses;
              league_points = raw.league_points;
              league_tier = raw.league_tier;
              league_wins = raw.league_wins;
              assists = raw.assists;
              multi_kill_max = raw.multi_kill_max;
              objective_assists = raw.objective_assists;
              structure_damage = raw.structure_damage;
              win_status = raw.win_status;
            };
          ban1 = Option.map2 raw.ban_id_1 raw.ban_1 ~f:(fun id name -> { id; name });
          ban2 = Option.map2 raw.ban_id_2 raw.ban_2 ~f:(fun id name -> { id; name });
          ban3 = Option.map2 raw.ban_id_3 raw.ban_3 ~f:(fun id name -> { id; name });
          ban4 = Option.map2 raw.ban_id_4 raw.ban_4 ~f:(fun id name -> { id; name });
        })
    |> Result.map_error ~f:Exn.to_string

  type t = { entries: player_x_match list } [@@deriving sexp, compare, equal, of_yojson] [@@unboxed]
end

module Make (M : Call.S) = struct
  let make_uri = Call.make_uri ~dev_id:M.dev_id ~auth_key:M.auth_key

  let get_match_details ~session ~match_id =
    let uri = make_uri ~endpoint:"getmatchdetails" ~session [ Types.Match_id.to_string match_id ] in
    Call.run `GET uri (Parse [%of_yojson: Match_player.t list])

  let get_match_details_batch ~session ~match_ids =
    let uri =
      make_uri ~endpoint:"getmatchdetails" ~session
        [ Array.of_list_map match_ids ~f:Types.Match_id.to_string |> String.concat_array ~sep:"," ]
    in
    Call.run `GET uri Unparsed
end
