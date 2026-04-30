# Fty base ---------------------------------------------------------------

df_fty_base <-
  tbl(db_con(), I("fty.fty_base_vw")) |>
  filter(season == cur_season) |>
  filter(!league_id %in% c(24608)) |> # DELTE
  arrange(str_to_lower(league_name), str_to_lower(competitor_name)) |>
  as_tibble() |>
  mutate(across(ends_with("_id"), \(x) as.integer(x)))


# Fty categories ---------------------------------------------------------

df_fty_cats <-
  tbl(db_con(), I("fty.fty_categories_vw")) |>
  filter(season == cur_season | is.na(league_id)) |>
  filter(!league_id %in% c(24608) | is.na(league_id)) |> # DELTE
  as_tibble() |>
  mutate(across(ends_with("_id"), \(x) as.integer(x)))

# Fty schedule -----------------------------------------------------------

dfs_fty_schedule <-
  tbl(db_con(), I("fty.fty_league_schedule_vw")) |>
  filter(season == cur_season) |>
  filter(!league_id %in% c(24608)) |> # DELETE
  as_tibble() |>
  mutate(
    across(matches("_id$|_period$"), \(x) as.integer(x)),
    matchup = str_c(matchup_period, " (", matchup_start, ")")
  ) |>
  (\(df) {
    bind_rows(
      df,
      distinct(df, league_id, season, platform, competitor_id) |>
        left_join(
          summarise(df, matchup_start = max(matchup_end) + ddays(1), .by = c(season, platform, league_id)) |>
            mutate(matchup_period = 99, matchup_end = as.Date("2999-01-01"), matchup = "Post Fantasy")
        )
    )
  })() |>
  nest_by(league_id) |>
  deframe()


# Fty roster -------------------------------------------------------------

dfs_fty_roster <-
  tbl(db_con(), I("fty.fty_team_roster_schedule_vw")) |>
  filter(season == cur_season) |>
  filter(!league_id %in% c(24608)) |> # DELETE
  filter(assigned_date < cur_date) |> # for testing purposes
  select(-c(competitor_name, opponent_name)) |>
  as_tibble() |>
  mutate(across(matches("_id$|_period$"), \(x) as.integer(x))) |>
  mutate(dow = wday(assigned_date, week_start = 1), .after = assigned_date) |>
  left_join(
    select(df_nba_season_segments, starts_with("season"), begin_date, end_date),
    by = join_by(season, assigned_date >= begin_date, assigned_date <= end_date)
  ) |>
  filter(season_type == "Regular Season") |>
  nest_by(league_id) |>
  deframe()

# Fantasy Box Scores -----------------------------------------------------

df_fty_box_score <-
  tbl(db_con(), I("fty.fty_matchup_box_score_vw")) |>
  filter(season == cur_season) |>
  filter(!league_id %in% c(24608)) |> # DELETE
  filter(matchup <= 11) |> # for tetsing purposes
  select(-season, -platform, -matches("r_name|r_abbrev")) |>
  relocate(starts_with("competitor"), .before = matchup) |>
  as_tibble() |>
  group_by(league_id, matchup) |>
  calc_z_pcts() |>
  ungroup() |>
  mutate(across(c(ends_with("_id"), matchup), \(x) as.integer(x)))


# Free Agents ------------------------------------------------------------

dfs_fty_free_agents <-
  tbl(db_con(), I("fty.fty_free_agents_vw")) |>
  filter(!league_id %in% c(24608)) |> # DELETE
  as_tibble() |>
  mutate(across(ends_with("_id"), \(x) as.integer(x))) |>
  nest_by(league_id) |>
  deframe()


# Recent Avtivity --------------------------------------------------------

dfs_fty_recent_activity <-
  tbl(db_con(), I("fty.fty_recent_activity_vw")) |>
  filter(season == cur_season) |>
  filter(!league_id %in% c(24608)) |> # DELETE
  select(league_id, competitor_id, competitor_name, player, action, timestamp) |>
  as_tibble() |>
  mutate(across(ends_with("_id"), \(x) as.integer(x))) |>
  arrange(desc(timestamp)) |>
  nest_by(league_id) |>
  deframe()


# League categories ------------------------------------------------------

ls_lo_lg_cats <-
  map(set_names(unique(na.omit(df_fty_cats$league_id))), \(x) {
    list(
      "Overall" = c("All Categories" = "all_cat"),
      # Order categories appropiately
      "Categories" = df_fty_cats |>
        filter(h2h_cat, league_id == x) |>
        select(fmt_category, nba_category) |>
        deframe(),
      "Z Scores" = c("Field Goal Z" = "fg_z", "Free Throw Z" = "ft_z")
    )
  })


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
  dfs_fty_schedule,
  dfs_fty_roster,
  dfs_fty_recent_activity,
  ls_lo_lg_cats,
  ls_fty_lookup,
  overwrite = TRUE
)
