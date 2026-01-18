# dfs_h2h_past <- df_fty_roster |>
#   filter(assigned_date < cur_date) |>
#   mutate(dow = lubridate::wday(assigned_date, week_start = 1), .after = assigned_date) |>
#   left_join(
#     select(df_nba_schedule, team, game_date, scheduled_to_play),
#     by = join_by(player_team == team, assigned_date == game_date)
#   ) |>
#   select(-c(season_type, begin_date, end_date)) |>
#   rename(game_date = assigned_date) |>
#   distinct() |>
#   nest_by(league_id) |>
#   deframe()

# # THINK DATES ARE FUCKED UP AT SOURCE

# usethis::use_data(
#   dfs_h2h_past,
#   overwrite = TRUE
# )

ct <- as.Date("2026-01-14")
mup <- 13
cid <- 25
oid <- pluck(dfs_fty_schedule, "95537") |>
  filter(competitor_id == cid, matchup_period == mup) |>
  pull(opponent_id)

# past
df_past <- pluck(dfs_fty_roster, "95537") |>
  filter(assigned_date < ct) |>
  mutate(dow = lubridate::wday(assigned_date, week_start = 1), .after = assigned_date) |>
  left_join(
    select(df_nba_schedule, team, game_date, scheduled_to_play),
    by = join_by(player_team == team, assigned_date == game_date)
  ) |>
  select(-c(season_type, season, begin_date, end_date, platform, player_acquisition_type)) |>
  rename(game_date = assigned_date) |>
  filter(matchup_period == mup, competitor_id %in% c(cid, oid)) |>
  left_join(
    df_player_box_score |>
      select(-c(matches("season|team"), player_name, game_id, espn_id, yahoo_id)),
    by = join_by(player_id, game_date)
  )

view(df_past)

# today
df_today <- pluck(dfs_fty_roster, "95537") |>
  filter(assigned_date == ct) |>
  mutate(dow = lubridate::wday(assigned_date, week_start = 1), .after = assigned_date) |>
  left_join(
    select(df_nba_schedule, team, game_date, scheduled_to_play),
    by = join_by(player_team == team, assigned_date == game_date)
  ) |>
  select(-c(season_type, season, begin_date, end_date, platform, player_acquisition_type)) |>
  rename(game_date = assigned_date) |>
  filter(matchup_period == mup, competitor_id %in% c(cid, oid)) |>
  left_join(
    pluck(dfs_rolling_stats, "7") |>
      slice_max(order_by = game_date, by = player_id) |>
      select(-c(player_name, game_date, starts_with("team_slug"))) |>
      rename_with(\(x) str_c(x, "_y")),
    by = join_by(player_id == player_id_y)
  ) |>
  mutate(across(ends_with("_y"), \(x) if_else(is.na(scheduled_to_play) | player_injury_status == "OUT", NA, x))) |>
  rename_with(\(x) str_remove(x, "_y"), ends_with("_y"))

view(df_today)

# future
df_future <- pluck(dfs_fty_roster, "95537") |>
  filter(assigned_date == ct) |>
  left_join(
    select(df_nba_schedule, team, game_date, scheduled_to_play) |>
      filter(game_date > ct),
    by = join_by(player_team == team),
    relationship = "many-to-many"
  ) |>
  inner_join(
    pluck(dfs_fty_schedule, "95537") |>
      filter(matchup_period == mup) |>
      select(starts_with("matchup")) |>
      distinct(),
    by = join_by(matchup_period, between(game_date, matchup_start, matchup_end))
  ) |>
  mutate(dow = lubridate::wday(game_date, week_start = 1)) |>
  select(
    -c(
      season_type,
      season,
      begin_date,
      end_date,
      platform,
      player_acquisition_type,
      matchup_start,
      matchup_end,
      assigned_date
    )
  ) |>
  filter(matchup_period == mup, competitor_id %in% c(cid, oid)) |>
  left_join(
    pluck(dfs_rolling_stats, "7") |>
      slice_max(order_by = game_date, by = player_id) |>
      select(-c(player_name, game_date, starts_with("team_slug"))) |>
      rename_with(\(x) str_c(x, "_y")),
    by = join_by(player_id == player_id_y)
  ) |>
  mutate(across(ends_with("_y"), \(x) if_else(is.na(scheduled_to_play) | player_injury_status == "OUT", NA, x))) |>
  rename_with(\(x) str_remove(x, "_y"), ends_with("_y"))

view(df_future)

df_h2h <- bind_rows(lst(df_past, df_today, df_future), .id = "origin")

view(df_h2h)
