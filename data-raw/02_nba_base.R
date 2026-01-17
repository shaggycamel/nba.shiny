# Player box score -------------------------------------------------------

df_player_box_score <-
  tbl(db_con(), I("nba.nba_player_box_score_vw")) |>
  filter(season >= prev_season) |>
  as_tibble() |> # following mutate opeations need tibble
  mutate(
    across(ends_with("_id"), \(x) as.double(x)),
    game_date = force_tz(game_date, tz = "NZ"),
    season = ordered(season),
    season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs")),
    year_season_type = fct_cross(season_type, str_sub(season, start = 6), sep = " ")
  )


# NBA Schedule -----------------------------------------------------------

df_nba_schedule <-
  tbl(db_con(), I("nba.nba_season_segments_vw")) |>
  filter(season == cur_season, season_type == "Regular Season") |>
  select(begin_date, end_date) |>
  as_tibble() |>
  pivot_longer(cols = ends_with("date"), values_to = "game_date") |>
  complete(game_date = seq.Date(min(game_date), max(game_date), by = "day")) |>
  select(game_date) |>
  left_join(
    tbl(db_con(), I("nba.nba_schedule_vw")) |>
      filter(season == cur_season) |>
      as_tibble(),
    by = join_by(game_date)
  ) |>
  mutate(
    season = cur_season,
    season_type = "Regular Season",
    # game_date = lubridate::force_tz(game_date, tz = "EST"), # THINK ERROR INDUCING
    scheduled_to_play = ifelse(!is.na(game_id), 1, game_id) # used in h2h calculations
  )


# Season segments --------------------------------------------------------

df_nba_season_segments <-
  tbl(db_con(), I("nba.nba_season_segments_vw")) |>
  filter(season >= prev_season) |>
  as_tibble() |>
  (\(df) {
    bind_rows(
      filter(df, cur_date > begin_date, cur_date < end_date) |>
        mutate(end_date = as.Date(cur_date)),

      setdiff(df, filter(df, cur_date > begin_date, cur_date < end_date))
    )
  })() |>
  mutate(mid_date = begin_date + (end_date - begin_date) / 2)


# Team roster ------------------------------------------------------------

df_nba_roster <- tbl(db_con(), I("nba.nba_latest_team_roster_vw")) |>
  filter(season == cur_season) |>
  as_tibble()


# Player rolling stats ---------------------------------------------------

# Probs move somewhere else
cats <- c("min", "fgm", "fga", "fg3_m", "ftm", "fta", "pts", "reb", "ast", "stl", "blk", "tov", "pf", "dd2", "td3")

dfs_rolling_stats <- df_player_box_score |>
  arrange(game_date) |>
  filter(game_date < cur_date) |>
  select(-c(season, season_type, year_season_type, game_id)) |>
  (\(df_t) {
    map(set_names(c(7, 15, 30)), \(window) {
      df_t |>
        mutate(
          across(any_of(cats), \(x) {
            slider::slide_period_dbl(x, game_date, "day", ~ mean(.x, na.rm = TRUE), .before = window, .after = -1)
          }),
          .by = player_id
        ) |>
        mutate(across(any_of(cats), \(x) coalesce(x, 0)))
    })
  })() |>
  map(\(df_t) {
    df_t |>
      bind_rows(
        df_nba_schedule |>
          filter(game_date > (cur_date - days(1))) |>
          left_join(
            select(df_nba_roster, player_id, espn_id, yahoo_id, player_name = player, team_slug),
            by = join_by(team == team_slug),
            relationship = "many-to-many"
          ) |>
          select(player_id, espn_id, yahoo_id, player_name, team_slug = team, game_date) |>
          left_join(
            slice_max(df_t, order_by = game_date, by = player_id) |>
              select(player_id, min:last_col()),
            by = join_by(player_id),
            relationship = "many-to-many"
          )
      )
  })

usethis::use_data(
  df_player_box_score,
  dfs_rolling_stats,
  overwrite = TRUE
)
