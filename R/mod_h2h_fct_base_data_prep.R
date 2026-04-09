base_data_prep <- function(input, selected, opponent) {
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

          "today" = if (input$future_only) {
            NULL
          } else {
            if (
              x != selected$competitor_id |
                as.integer(input$matchup) < selected$cur_matchup_period
            ) {
              pluck(dfs_h2h_today, l_id, input$matchup, x, input$window)
            } else {
              pluck(dfs_h2h_today, l_id, input$matchup, x, input$window)
              # filter(!player_id %in% (as.integer(input$ex_player) %||% 0)) |>
              # bind_rows(
              #   dfs_h2h_today |>
              #     pluck(l_id, "free_agent", "free_agent", input$window)
              #   # filter(player_id %in% (as.integer(input$add_player) %||% 0))
              # )
            }
          },

          "future" = if (
            x != selected$competitor_id |
              as.integer(input$matchup) < selected$cur_matchup_period
          ) {
            pluck(dfs_h2h_future, l_id, input$matchup, x, input$window)
          } else {
            pluck(dfs_h2h_future, l_id, input$matchup, x, input$window)
            # filter(!player_id %in% (as.integer(input$ex_player) %||% 0)) |>
            # bind_rows(
            #   dfs_h2h_future |>
            #     pluck(l_id, "free_agent", "free_agent", input$window)
            #   # filter(player_id %in% (as.integer(input$add_player) %||% 0))
            # )
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
