grey_player_data_prep <- function(input, df_base, selected, opponent) {
  # past roster
  df_base |>
    filter(tense == "past") |>
    summarise(
      min_grey_date = min(game_date),
      max_grey_date = max(game_date),
      .by = player_id
    ) |>
    inner_join(
      # Current Roster
      pluck(dfs_fty_roster, as.character(selected$league_id)) |>
        filter(
          competitor_id %in% c(selected$competitor_id, opponent()$id),
          matchup_period == as.integer(input$matchup)
        ) |>
        mutate(max_assigned_date = max(assigned_date)) |>
        filter(
          min(assigned_date) != matchup_start |
            max(assigned_date) != max_assigned_date,
          .by = player_id
        ) |>
        distinct(player_id, matchup_start, max_assigned_date),
      by = join_by(player_id)
    ) |>
    mutate(
      min_grey_date = if_else(
        min_grey_date == matchup_start,
        NA_Date_,
        min_grey_date
      ),
      max_grey_date = if_else(
        between(max_grey_date, max_assigned_date - 1, max_assigned_date),
        NA_Date_,
        max_grey_date
      )
    ) |>
    select(-c(matchup_start, max_assigned_date))
}


table_data_prep <- function(df_base, selected, df_grey_player, pin_ix) {
  df_base |>
    arrange(game_date) |>
    select(competitor, player_team, player_id, player_name, inj_status, fmt_date, scheduled_to_play) |>
    distinct() |>
    mutate(
      scheduled_to_play = as.character(replace_na(scheduled_to_play, 0)),
      scheduled_to_play = if_else(
        inj_status == "Out",
        str_c(scheduled_to_play, "*"),
        scheduled_to_play,
        missing = scheduled_to_play
      )
    ) |>
    select(-inj_status) |>
    pivot_wider(
      names_from = fmt_date,
      values_from = scheduled_to_play,
      values_fill = "0"
    ) |>
    select(-starts_with("NA")) |>
    rowwise() |>
    mutate(
      games_remaining = if (selected$ui_date > unique(na.omit(df_base$matchup_end))) {
        0
      } else {
        sum(as.numeric(str_remove(c_across((pin_ix + 4):last_col()), "\\*")), na.rm = TRUE)
      },
      .before = if (all(is.na(df_base$matchup_end_plus))) last_col() else last_col(2)
    ) |>
    ungroup() |>
    left_join(df_grey_player, by = join_by(player_id))
}

table_sum_data_prep <- function(df_tbl, df_base, pin_ix) {
  df_tbl |>
    summarise(across(contains("/"), \(x) sum(as.numeric(x), na.rm = TRUE)), .by = competitor) |>
    arrange(desc(competitor)) |>
    rename(player_team = competitor) |>
    mutate(player_name = NA, .after = player_team) |>
    rowwise() |>
    mutate(
      games_remaining = if (cur_date > unique(na.omit(df_base$matchup_end))) {
        0
      } else {
        sum(as.numeric(str_remove(c_across((pin_ix + 2):last_col()), "\\*")), na.rm = TRUE)
      },
      .before = if (all(is.na(df_base$matchup_end_plus))) last_col() else last_col(2)
    ) |>
    ungroup() |>
    mutate(min_grey_date = NA_Date_, max_grey_date = NA_Date_)
}
