# Player Comparison ------------------------------------------------------

dfs_player_comparison <- map(set_names(names(dfs_free_agents)), \(lg) {
  map(set_names(names(dfs_rolling_stats)), \(rl) {
    df_inr <- dfs_rolling_stats[[rl]] |>
      filter(season == cur_season) |>
      slice_max(game_date, by = player_id)

    df_inr |>
      mutate(
        across(min:blk, \(x) round(scales::rescale(x), 2)),
        across(pf:td3, \(x) round(scales::rescale(x), 2)),
        tov = round((((tov * -1) - min(tov, na.rm = TRUE)) / (max(tov, na.rm = TRUE) - min(tov, na.rm = TRUE))) + 1, 2),
      ) |>
      calc_z_pcts() |>
      mutate(across(ends_with("_z"), \(x) round(scales::rescale(x), 2))) |>
      pivot_longer(
        cols = df_fty_cats |>
          filter(
            (league_id == lg | is.na(league_id)),
            ((h2h_cat & !str_like(nba_category, "%_pct")) |
              str_like(nba_category, "%_z"))
          ) |>
          pull(nba_category),
        names_to = "stat"
      ) |>
      (\(x) {
        bind_rows(
          slice_max(x, value, n = 3, by = player_id) |>
            mutate(performance = "Excels At") |>
            filter(value > 0),

          slice_min(x, value, n = 3, by = c(player_id, player_name)) |>
            mutate(performance = "Weak At")
        )
      })() |>
      mutate(stat_value = paste0(stat, " (", round(value, 2), ")")) |>
      summarise(
        stat_value = paste(stat_value, collapse = "\n"),
        .by = c(player_id, player_name, team, inj_status, performance)
      ) |>
      pivot_wider(names_from = performance, values_from = stat_value) |>
      left_join(
        df_inr |>
          select(player_id, any_of(unique(df_fty_cats$nba_category))),
        by = join_by(player_id)
      ) |>
      calc_z_pcts() |>
      left_join(
        dfs_free_agents[[lg]] |>
          select(player_id) |>
          mutate(free_agent = TRUE),
        by = join_by(player_id)
      ) |>
      left_join(
        tibble(team = names(ls_nba_teams), team_id = ls_nba_teams),
        by = join_by(team)
      ) |>
      relocate(team_id, .before = team) |>
      arrange(desc(min))
  })
})


# Nested injuries --------------------------------------------------------

min_inj_date <- as.Date(cur_date - days(30))
df_ns_injuries <-
  tbl(db_con(), I("nba.injuries")) |>
  filter(game_date >= min_inj_date, status == "Out") |>
  as_tibble() |>
  mutate(
    across(ends_with("_id"), \(x) as.integer(x)),
    opponent = str_remove(matchup, "@"),
    opponent = str_remove(opponent, team_slug),
    opponent = str_squish(opponent)
  ) |>
  left_join(
    df_nba_roster |>
      select(nba_id = player_id, salary) |>
      distinct(),
    by = join_by(nba_id)
  ) |>
  select(team = team_slug, opponent, game_date, player_name, salary) |>
  arrange(desc(game_date), desc(salary)) |>
  summarise(player_names = paste(player_name, collapse = ", "), .by = c(team, opponent, game_date))

ls_injuries <-
  map(set_names(names(dfs_rolling_stats)), \(x) {
    df_ns_injuries |>
      filter(game_date >= max(game_date) - days(x)) |>
      nest_by(team, .keep = TRUE) |>
      deframe()
  })


usethis::use_data(dfs_player_comparison, ls_injuries, overwrite = TRUE)
