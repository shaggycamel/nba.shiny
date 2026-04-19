base_data_prep <- function(input, selected, opponent, rv_alter_team) {
  #
  # Reactive add/exclude
  entries <- reactiveValuesToList(rv_alter_team)
  if (length(entries) > 0 && !is.null(input$ui_date)) {
    df_alter <- tibble(
      action = map_chr(entries, "action"),
      player_id = map_int(entries, "player_id"),
      action_date = as.Date(map_chr(entries, "action_date"))
    )
    print(df_alter)

    valid_combo <- \(df) if (nrow(df) > 0) df else NULL
    add <- valid_combo(filter(df_alter, action == "add", action_date >= input$ui_date))
    ex <- valid_combo(filter(df_alter, action == "ex", action_date >= input$ui_date))
  } else {
    add <- ex <- NULL
  }

  l_id <- as.character(selected$league_id)
  map(as.character(c(opponent()$id, selected$competitor_id)), \(x) {
    bind_rows(
      compact(
        lst(
          "past" = if (input$future_only) {
            NULL
          } else {
            pluck(dfs_h2h_past, l_id, input$matchup, x)
          },

          "future" = if (
            x != selected$competitor_id |
              as.integer(input$matchup) < selected$cur_matchup_period
          ) {
            pluck(dfs_h2h_future, l_id, input$matchup, x, input$window)
          } else {
            pluck(dfs_h2h_future, l_id, input$matchup, x, input$window) |>
              (\(df) {
                # fmt: skip
                if (!is.null(ex)) df <- anti_join(df, ex, by = join_by(player_id, assigned_date >= action_date))
                if (!is.null(add)) {
                  df <- df |>
                    bind_rows(
                      dfs_h2h_future |>
                        pluck(l_id, "free_agent", "free_agent", input$window) |>
                        inner_join(add, by = join_by(player_id, assigned_date >= action_date))
                    )
                }
                df
              })()
          }
        )
      ),
      .id = "tense"
    ) |>
      mutate(competitor = pluck(ls_fty_lookup, "cp_id_to_name", l_id, x))
  }) |>
    list_rbind() |>
    mutate(
      competitor = ordered(
        competitor,
        c(
          pluck(ls_fty_lookup, "cp_id_to_name", l_id, as.character(opponent()$id)),
          pluck(ls_fty_lookup, "cp_id_to_name", l_id, as.character(selected$competitor_id))
        )
      )
    )
}
