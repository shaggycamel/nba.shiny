dfs_h2h_past <- dfs_fty_roster |>
  list_rbind(names_to = "league_id") |>
  filter(assigned_date < cur_date) |>
  left_join(
    select(df_nba_schedule, team, game_date, scheduled_to_play),
    by = join_by(player_team == team, assigned_date == game_date)
  ) |>
  select(-c(season_type, begin_date, end_date, player_acquisition_type, player_injury_status)) |>
  rename(game_date = assigned_date) |>
  left_join(
    df_nba_player_box_score |>
      select(-c(contains("season"), espn_id, yahoo_id, player_name)),
    by = join_by(player_id, game_date, player_team == team)
  ) |>
  mutate(fmt_date = format(game_date, "%d/%m")) |>
  distinct() |> # FOR SAFETY
  nest_by(league_id, matchup_period, competitor_id) |>
  nest_by(league_id, matchup_period) |>
  nest_by(league_id) |>
  deframe() |>
  map(\(x) map(deframe(x), \(y) deframe(y)))


dfs_today_future_generation <- function(timeframe) {
  #
  # fty roster & free agents
  df <- dfs_fty_roster |>
    list_rbind(names_to = "league_id") |>
    slice_max(assigned_date, by = c(league_id, competitor_id)) |>
    mutate(competitor_id = as.character(competitor_id)) |>
    select(league_id, platform, competitor_id, player_id, player_name, player_team) |>
    bind_rows(
      dfs_fty_free_agents |>
        list_rbind(names_to = "league_id") |>
        mutate(competitor_id = "free_agent") |>
        select(league_id, platform, competitor_id, player_id, player_name, player_team)
    ) |>

    # fty schedule
    left_join(
      dfs_fty_schedule |>
        list_rbind(names_to = "league_id") |>
        filter(
          if (timeframe == "today") {
            matchup_start <= cur_date & cur_date <= matchup_end
          } else {
            cur_date <= matchup_end
          }
        ) |>
        mutate(competitor_id = as.character(competitor_id)) |>
        select(league_id, platform, matchup_period, matchup_start, matchup_end, competitor_id, opponent_id),
      by = join_by(league_id, platform, competitor_id),
      relationship = "many-to-many"
    ) |>
    mutate(
      matchup_period = as.character(matchup_period),
      matchup_period = if_else(competitor_id == "free_agent", "free_agent", matchup_period),
      scheduled_to_play = 1
    ) |>
    mutate(
      matchup_start = replace_na(matchup_start, min(matchup_start, na.rm = TRUE)),
      matchup_end = replace_na(matchup_end, max(matchup_end, na.rm = TRUE)),
      .by = league_id
    ) |>

    # Rolling stats
    left_join(
      dfs_rolling_stats |>
        list_rbind(names_to = "window") |>
        filter(if (timeframe == "today") game_date == cur_date else game_date > cur_date) |>
        select(
          window,
          player_id,
          inj_status,
          inj_reason,
          game_date,
          dow,
          game_id,
          team,
          opponent,
          home,
          min,
          pts,
          fg3_m,
          fgm,
          fga,
          ftm,
          fta,
          ast,
          reb,
          stl,
          blk,
          tov,
          pf,
          dd2,
          td3
        ),
      by = join_by(player_id, player_team == team, matchup_start <= game_date, matchup_end >= game_date),
      relationship = "many-to-many"
    ) |>
    select(-c(matchup_start, matchup_end)) |>
    mutate(fmt_date = format(game_date, "%d/%m")) |>
    distinct() # FOR SAFETY

  if (nrow(df) == 0) {
    df
  } else {
    df |>
      nest_by(league_id, matchup_period, competitor_id, window) |>
      nest_by(league_id, matchup_period, competitor_id) |>
      nest_by(league_id, matchup_period) |>
      nest_by(league_id) |>
      deframe() |>
      map(\(x) map(deframe(x), \(y) map(deframe(y), \(z) deframe(z))))
  }
}

# today/future
dfs_h2h_today <- dfs_today_future_generation("today")
dfs_h2h_future <- dfs_today_future_generation("future")

# Write data
usethis::use_data(
  dfs_h2h_past,
  dfs_h2h_today,
  dfs_h2h_future,
  overwrite = TRUE
)
