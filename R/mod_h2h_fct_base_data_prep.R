base_data_prep <- function(input, selected, opponent, rv_alter_team) {
  entries <- reactiveValuesToList(rv_alter_team)
  if (length(entries) > 0) {
    df_alter <- tibble(
      action = map_chr(entries, "action"),
      player_id = map_int(entries, "player_id"),
      action_date = as.Date(map_chr(entries, "action_date"))
    )

    valid_combo <- \(df) if (nrow(df) > 0) df else NULL
    add <- valid_combo(filter(df_alter, action == "add", action_date >= cur_date))
    ex <- valid_combo(filter(df_alter, action == "ex", action_date >= cur_date))
  } else {
    add <- ex <- NULL
  }

  l_id <- as.character(selected$league_id)
  map(na.omit(as.character(c(opponent()$id, selected$competitor_id))), \(x) {
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
                if (!is.null(ex)) df <- anti_join(df, ex, by = join_by(player_id, game_date >= action_date))
                if (!is.null(add)) {
                  df <- df |>
                    bind_rows(
                      dfs_h2h_future |>
                        pluck(l_id, "free_agent", "free_agent", input$window) |>
                        inner_join(add, by = join_by(player_id, game_date >= action_date))
                    )
                }
                df
              })()
          }
        )
      ),
      .id = "tense"
    ) |>
      mutate(
        competitor_id = as.integer(x),
        competitor = pluck(ls_fty_lookup, "cp_id_to_name", l_id, x)
      )
  }) |>
    list_rbind() |>
    mutate(
      competitor = if (is.na(opponent()$id)) {
        pluck(ls_fty_lookup, "cp_id_to_name", l_id, as.character(selected$competitor_id))
      } else {
        ordered(
          competitor,
          c(
            pluck(ls_fty_lookup, "cp_id_to_name", l_id, as.character(opponent()$id)),
            pluck(ls_fty_lookup, "cp_id_to_name", l_id, as.character(selected$competitor_id))
          )
        )
      }
    )
}
