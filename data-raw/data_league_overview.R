# League overview dataframes ---------------------------------------------

dfs_league_overview <-
  df_fty_box_score |>
  left_join(
    df_fty_box_score |>
      mutate(
        across(
          any_of(df_fty_cats$nba_category),
          \(x) if (cur_column() == "tov") percent_rank(-x) else percent_rank(x),
          .names = "{.col}-perc_rank"
        ),
        .by = c(league_id, matchup)
      ) |>
      rename_with(
        \(x) paste0(x, "-value"),
        .cols = any_of(df_fty_cats$nba_category)
      ) |>
      pivot_longer(
        matches(paste(df_fty_cats$nba_category, collapse = "|")),
        names_to = c("category", ".value"),
        names_sep = "-"
      ) |>
      inner_join(
        select(df_fty_cats, league_id, nba_category) |>
          filter(!str_detect(nba_category, "[g|t][m|a]")),
        by = join_by(league_id, category == nba_category)
      ) |>
      arrange(category) |>
      mutate(all_cat_text = paste0(category, ": ", round(value, 2), " (", round(perc_rank, 2), ")")) |>
      summarise(
        all_cat = sum(perc_rank, na.rm = TRUE),
        all_cat_text = paste(all_cat_text, collapse = "\n"),
        .by = c(league_id, competitor_id, matchup)
      ) |>
      mutate(all_cat_text = paste("<b>Total -", round(all_cat, 2), "</b>", "\n", all_cat_text)),
    by = join_by(league_id, competitor_id, matchup)
  ) |>
  mutate(
    across(
      any_of(c(df_fty_cats$nba_category, "matchup")),
      \(x) lead(x, order_by = matchup),
      .names = "{.col}_lead"
    ),
    .by = c(league_id, competitor_id)
  ) |>
  mutate(
    across(
      any_of(df_fty_cats$nba_category),
      \(x) if (cur_column() == "tov") rank(x) else rank(-x),
      .names = "{.col}_rank"
    ),
    .by = c(league_id, matchup)
  ) |>
  mutate(
    across(
      any_of(str_c(df_fty_cats$nba_category, "_rank")),
      \(x) lead(x, order_by = matchup),
      .names = "{.col}_lead"
    ),
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
              sigmoid(matchup_sigmoid),
              to = c(df_t[[stat]], df_t[[str_c(stat, "_lead")]])
            )
            stat_rank_sigmoid <- scales::rescale(
              sigmoid(matchup_sigmoid),
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
        select(df_tmp, -all_cat_text) |>
          filter(matchup == max(matchup), .by = league_id)
      ) |>
      left_join(
        select(df_tmp, league_id, competitor_id, matchup, all_cat_text),
        by = join_by(league_id, competitor_id, matchup)
      )
  })() |>
  mutate(matchup_sigmoid = if_else(is.na(matchup_sigmoid), matchup, matchup_sigmoid)) |>
  (\(df) {
    cols <- intersect(colnames(df), df_fty_cats$nba_category) |>
      purrr::discard(\(x) str_detect(x, "[g|t][m|a]|all_cat")) # explicitly referenced on purpose

    bind_cols(
      df,
      map(set_names(cols), \(x) {
        paste0(x, ": ", round(pull(df, x), 2), " (", round(pull(df, !!sym(paste0(x, "_rank"))), 2), ")")
      }) |>
        bind_cols() |>
        rename_with(\(x) paste0(x, "_text"))
    )
  })() |>
  left_join(
    select(df_fty_base, league_id, competitor_id, competitor_name),
    by = join_by(league_id, competitor_id)
  ) |>
  mutate(
    across(ends_with("_text"), \(x) paste0("<b>", matchup, " - ", competitor_name, "</b>", "<br>", x)),
    .by = c(league_id, matchup, competitor_id)
  ) |>
  select(-ends_with("_lead")) |>
  nest_by(league_id) |>
  deframe()


# Write data -------------------------------------------------------------

usethis::use_data(dfs_league_overview, overwrite = TRUE)
