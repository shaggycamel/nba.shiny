# Fty base ---------------------------------------------------------------

df_fty_base <-
  tbl(db_con(), I("fty.fty_base_vw")) |>
  filter(season == cur_season) |>
  arrange(str_to_lower(league_name), str_to_lower(competitor_name)) |>
  as_tibble() |>
  mutate(across(ends_with("_id"), \(x) as.integer(x)))


# Fty categories ---------------------------------------------------------

df_fty_cats <-
  tbl(db_con(), I("fty.fty_categories_vw")) |>
  filter(season == cur_season | is.na(league_id)) |>
  as_tibble() |>
  mutate(across(ends_with("_id"), \(x) as.integer(x)))


# Fty schedule -----------------------------------------------------------

dfs_fty_schedule <-
  tbl(db_con(), I("fty.fty_league_schedule_vw")) |>
  filter(season == cur_season) |>
  as_tibble() |>
  mutate(
    across(matches("_id$|_period$"), \(x) as.integer(x)),
    matchup = str_c(matchup_period, " (", matchup_start, ")")
  ) |>
  nest_by(league_id) |>
  deframe()


# Fty roster -------------------------------------------------------------

dfs_fty_roster <-
  tbl(db_con(), I("fty.fty_team_roster_schedule_vw")) |>
  filter(season == cur_season) |>
  select(-c(competitor_name, opponent_name)) |>
  as_tibble() |>
  mutate(across(matches("_id$|_period$"), \(x) as.integer(x))) |>
  left_join(
    select(df_nba_season_segments, starts_with("season"), begin_date, end_date),
    by = join_by(season, assigned_date >= begin_date, assigned_date <= end_date)
  ) |>
  filter(season_type == "Regular Season") |>
  nest_by(league_id) |>
  deframe()


# Free Agents ------------------------------------------------------------

dfs_free_agents <-
  tbl(db_con(), I("fty.fty_free_agents_vw")) |>
  as_tibble() |>
  mutate(across(ends_with("_id"), \(x) as.integer(x))) |>
  nest_by(league_id) |>
  deframe()


# Conversion list --------------------------------------------------------

ls_fty_lookup <- list(
  "lg_name_to_id" = as.list(deframe(distinct(df_fty_base, league_name, league_id))),
  "lg_id_to_name" = as.list(deframe(distinct(df_fty_base, league_id, league_name))),
  "lg_id_to_platform" = as.list(deframe(distinct(df_fty_base, league_id, platform))),
  "cp_id_to_name" = select(df_fty_base, league_id, competitor_id, competitor_name) |>
    nest_by(league_id) |>
    mutate(data = list(as.list(deframe(data)))) |>
    deframe(),
  "cp_name_to_id" = select(df_fty_base, league_id, competitor_name, competitor_id) |>
    nest_by(league_id) |>
    mutate(data = list(as.list(deframe(data)))) |>
    deframe()
)

usethis::use_data(
  df_fty_base,
  df_fty_cats,
  dfs_fty_schedule,
  dfs_fty_roster,
  ls_fty_lookup,
  overwrite = TRUE
)
