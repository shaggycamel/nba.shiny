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


# Fantasy Box Scores -----------------------------------------------------

df_fty_box_score <-
  tbl(db_con(), I("fty.fty_matchup_box_score_vw")) |>
  filter(season == cur_season) |>
  select(-season, -platform, -matches("r_name|r_abbrev")) |>
  relocate(starts_with("competitor"), .before = matchup) |>
  group_by(league_id, matchup) |>
  calc_z_pcts() |>
  ungroup() |>
  as_tibble() |>
  mutate(across(c(ends_with("_id"), matchup), \(x) as.integer(x)))


# League overview dataframes ---------------------------------------------

dfs_league_overview <-
  df_fty_box_score |>
  left_join(
    df_fty_box_score |>
      mutate(
        across(any_of(discard(cats, \(x) str_detect(x, "_z|[t|g][m|a]"))), \(x) percent_rank(x)),
        .by = c(league_id, matchup)
      ) |>
      mutate(tov = 1 - tov) |> # Reverse turnover distribution | Eventually fix this in any hover boxes
      pivot_longer(
        any_of(discard(cats, \(x) str_detect(x, "_z|[t|g][m|a]"))),
        names_to = "stat",
        values_to = "perc_rank"
      ) |>
      summarise(all_cat = sum(perc_rank, na.rm = TRUE), .by = c(league_id, competitor_id, matchup)),
    by = join_by(league_id, competitor_id, matchup)
  ) |>
  mutate(
    across(any_of(c(df_fty_cats$nba_category, "matchup")), \(x) lead(x, order_by = matchup), .names = "{.col}_lead"),
    .by = c(league_id, competitor_id)
  ) |>
  mutate(
    across(any_of(discard(df_fty_cats$nba_category, \(x) x == "tov")), \(x) rank(x * -1), .names = "{.col}_rank"),
    .by = c(league_id, matchup)
  ) |>
  mutate(tov_rank = rank(tov), .by = c(league_id, matchup)) |>
  mutate(
    across(any_of(str_c(df_fty_cats$nba_category, "_rank")), \(x) lead(x, order_by = matchup), .names = "{.col}_lead"),
    .by = c(league_id, competitor_id)
  ) |>
  mutate(across(where(is.numeric), \(x) as.double(replace_na(x, 0)))) |>
  (\(df_tmp) {
    filter(df_tmp, matchup < max(matchup), .by = league_id) |>
      group_by(league_id, competitor_id, matchup) |>
      group_modify(.keep = TRUE, \(df_t, ...) {
        x <- seq(-5, 5, 0.3)
        df_ls <- list()

        # To handle when df is empty, ie - start of season
        if (nrow(df_t) > 0) {
          for (stat in intersect(df_fty_cats$nba_category, colnames(df_t))) {
            matchup_sigmoid <- x
            if (df_t[[stat]] > df_t[[str_c(stat, "_lead")]]) {
              matchup_sigmoid <- rev(matchup_sigmoid)
            }
            stat_sigmoid <- scales::rescale(
              pracma::sigmoid(matchup_sigmoid),
              to = c(df_t[[stat]], df_t[[str_c(stat, "_lead")]])
            )
            stat_rank_sigmoid <- scales::rescale(
              pracma::sigmoid(matchup_sigmoid),
              to = c(df_t[[str_c(stat, "_rank")]], df_t[[str_c(stat, "_rank_lead")]])
            )
            matchup_sigmoid <- scales::rescale(matchup_sigmoid, to = c(df_t$matchup, df_t$matchup_lead))

            df_ls <- df_ls |>
              append(list(tibble(
                matchup_sigmoid = matchup_sigmoid,
                stat = stat,
                sigmoid = stat_sigmoid,
                rank_sigmoid = as.double(stat_rank_sigmoid)
              )))
          }
        }

        if (length(df_ls) == 0) {
          tibble(
            matchup_sigmoid = double(),
            stat = character(),
            sigmoid = double(),
            rank_sigmoid = double()
          )
        } else {
          bind_rows(df_ls)
        }
      }) |>
      ungroup() |>
      filter(!(as.integer(matchup_sigmoid) == matchup_sigmoid & matchup != matchup_sigmoid)) |>
      pivot_wider(names_from = stat, values_from = c(sigmoid, rank_sigmoid)) |>
      rename_with(\(x) str_remove(x, "sigmoid_"), .cols = starts_with("sigmoid_")) |>
      rename_with(\(x) str_c(str_remove(x, "rank_sigmoid_"), "_rank"), .cols = starts_with("rank_sigmoid_")) |>
      bind_rows(
        df_tmp |>
          filter(matchup == max(matchup), .by = league_id)
      )
  })() |>
  mutate(matchup_sigmoid = if_else(is.na(matchup_sigmoid), matchup, matchup_sigmoid)) |>
  left_join(
    df_fty_base |>
      select(league_id, competitor_id, competitor_name),
    by = join_by(league_id, competitor_id)
  ) |>
  nest_by(league_id) |>
  deframe()


usethis::use_data(
  dfs_league_overview,
  ls_lo_lg_cats,
  overwrite = TRUE
)
