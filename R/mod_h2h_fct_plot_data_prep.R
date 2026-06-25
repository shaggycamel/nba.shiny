plot_data_prep <- function(df_base, rv_carry_thru) {
  df_base |>
    select(
      competitor,
      player_id,
      player_name,
      inj_status,
      matches("f[g|t][m|a]"),
      any_of(unname(pluck(ls_lo_lg_cats, as.character(rv_carry_thru$league_id))[["Categories"]]))
    ) |>
    pivot_longer(c(
      matches("f[g|t][m|a]"),
      any_of(unname(pluck(ls_lo_lg_cats, as.character(rv_carry_thru$league_id))[["Categories"]]))
    )) |>
    mutate(value = if_else(inj_status == "Out", 0, value, missing = value)) |>
    summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(competitor, player_id, player_name, name)
    ) |>
    arrange(desc(value)) |>
    (\(x) {
      f_inr <- \(col_string) {
        m_col <- sym(str_c(col_string, "m"))
        a_col <- sym(str_c(col_string, "a"))

        x |>
          filter(str_detect(name, col_string)) |>
          pivot_wider(names_from = name, values_from = value) |>
          summarise(
            #fmt: skip
            label = paste0(
              "Total - ", round(sum(!!m_col, na.rm = TRUE), 0), "/", round(sum(!!a_col, na.rm = TRUE), 0), " (", label_percent(accuracy = 0.01)(sum(!!m_col, na.rm = TRUE)/sum(!!a_col, na.rm = TRUE)),")\n\n",
              paste(str_c(player_name, " - ", round(!!m_col, 0), "/", round(!!a_col, 0)), collapse = "\n")
            ),
            name = str_c(col_string, "_pct"),
            value = sum(!!m_col, na.rm = TRUE) / sum(!!a_col, na.rm = TRUE),
            .by = competitor
          )
      }

      bind_rows(
        f_inr("fg"),
        f_inr("ft"),
        x |>
          filter(!str_like(name, "f[g|t][m|a]")) |>
          summarise(
            # fmt: skip
            label = paste0(
                  "Total - ", round(sum(value, na.rm = TRUE), 1), "\n\n",
                  paste(str_c(player_name, " - ", round(value, 1)), collapse = "\n")
                ),
            value = sum(value, na.rm = TRUE),
            .by = c(competitor, name)
          )
      )
    })() |>
    mutate(
      name = ordered(name, c("reb", "blk", "stl", "tov", "ast", "pts", "fg3_m", "fg_pct", "ft_pct", "dd2", "td3")),
      name = fct_drop(name)
    )
  # CHECK THIS ORDER IS REETAINED IN PLOT
}
