df_player_box_score <-
  nba.dataRub::dh_getQuery(db_con, "SELECT * FROM nba_player_box_score_vw") |>
  # tbl(db_con, I("nba_player_box_score_vw")) |>
  filter(season >= prev_season) |>
  mutate(
    game_date = force_tz(game_date, tz = "NZ"),
    season = ordered(season),
    season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs")),
    year_season_type = fct_cross(season_type, str_sub(season, start = 6), sep = " ")
  )

usethis::use_data(df_player_box_score, overwrite = TRUE)
