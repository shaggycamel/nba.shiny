dfs_h2h_past <- df_fty_roster |>
  filter(assigned_date < cur_date) |>
  mutate(dow = lubridate::wday(assigned_date, week_start = 1), .after = assigned_date) |>
  left_join(
    select(df_nba_schedule, team, game_date, scheduled_to_play),
    by = join_by(player_team == team, assigned_date == game_date)
  ) |>
  select(-c(season_type, begin_date, end_date)) |>
  rename(game_date = assigned_date) |>
  distinct() |>
  nest_by(league_id) |>
  deframe()

# THINK DATES ARE FUCKED UP AT SOURCE

usethis::use_data(
  dfs_h2h_past,
  overwrite = TRUE
)
