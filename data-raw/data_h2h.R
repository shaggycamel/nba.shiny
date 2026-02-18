dfs_h2h_past <- dfs_fty_roster |>
  list_rbind(names_to = "league_id") |>
  left_join(
    select(df_nba_schedule, team, game_date, scheduled_to_play),
    by = join_by(player_team == team, assigned_date == game_date)
  ) |>
  select(-c(season_type, begin_date, end_date, player_acquisition_type, player_injury_status)) |>
  rename(game_date = assigned_date) |>
  left_join(
    df_player_box_score |>
      select(-c(contains("season"), espn_id, yahoo_id, player_name)),
    by = join_by(player_id, game_date, player_team == team)
  ) |>
  mutate(fmt_date = format(game_date, "%d/%m")) |>
  nest_by(league_id, matchup_period, competitor_id) |>
  nest_by(league_id, matchup_period) |>
  nest_by(league_id) |>
  deframe() |>
  map(\(x) map(deframe(x), \(y) deframe(y)))


dfs_today_future_generation <- function(timeframe) {
  #
  # fty roster & free agents
  df <- dfs_fty_roster |>
    list_rbind(names_to = "league_id") |>
    slice_max(assigned_date, by = c(league_id, competitor_id)) |>
    mutate(competitor_id = as.character(competitor_id)) |>
    select(league_id, platform, competitor_id, player_id, player_name, player_team) |>
    bind_rows(
      dfs_free_agents |>
        list_rbind(names_to = "league_id") |>
        mutate(competitor_id = "free_agent") |>
        select(league_id, platform, competitor_id, player_id, player_name, player_team)
    ) |>

    # fty schedule
    left_join(
      dfs_fty_schedule |>
        list_rbind(names_to = "league_id") |>
        filter(
          if (timeframe == "today") {
            matchup_start <= cur_date & cur_date <= matchup_end
          } else {
            cur_date <= matchup_end
          }
        ) |>
        mutate(competitor_id = as.character(competitor_id)) |>
        select(league_id, platform, matchup_period, matchup_start, matchup_end, competitor_id, opponent_id),
      by = join_by(league_id, platform, competitor_id),
      relationship = "many-to-many"
    ) |>
    mutate(
      matchup_period = as.character(matchup_period),
      matchup_period = if_else(competitor_id == "free_agent", "free_agent", matchup_period),
      scheduled_to_play = 1
    ) |>
    mutate(
      matchup_start = replace_na(matchup_start, min(matchup_start, na.rm = TRUE)),
      matchup_end = replace_na(matchup_end, max(matchup_end, na.rm = TRUE)),
      .by = league_id
    ) |>

    # Rolling stats
    left_join(
      dfs_rolling_stats |>
        list_rbind(names_to = "window") |>
        filter(if (timeframe == "today") game_date == cur_date else game_date > cur_date) |>
        select(
          window,
          player_id,
          inj_status,
          inj_reason,
          game_date,
          dow,
          game_id,
          team,
          opponent,
          home,
          min,
          pts,
          fg3_m,
          fgm,
          fga,
          ftm,
          fta,
          ast,
          reb,
          stl,
          blk,
          tov,
          pf,
          dd2,
          td3
        ),
      by = join_by(player_id, player_team == team, matchup_start <= game_date, matchup_end >= game_date),
      relationship = "many-to-many"
    ) |>
    select(-c(matchup_start, matchup_end)) |>
    mutate(fmt_date = format(game_date, "%d/%m"))

  if (nrow(df) == 0) {
    df
  } else {
    df |>
      nest_by(league_id, matchup_period, competitor_id, window) |>
      nest_by(league_id, matchup_period, competitor_id) |>
      nest_by(league_id, matchup_period) |>
      nest_by(league_id) |>
      deframe() |>
      map(\(x) map(deframe(x), \(y) map(deframe(y), \(z) deframe(z))))
  }
}

# today/future
dfs_h2h_today <- dfs_today_future_generation("today")
dfs_h2h_future <- dfs_today_future_generation("future")


# Write data
usethis::use_data(
  dfs_h2h_past,
  dfs_h2h_today,
  dfs_h2h_future,
  overwrite = TRUE
)


library(plotly)

league_id = "1966813226"
competitor_id = "5"
opponent_id = "6"
cur_matchup_period = "17"
window = "7"

df <- bind_rows(
  bind_rows(
    lst(
      "past" = pluck(dfs_h2h_past, league_id, cur_matchup_period, competitor_id),
      "today" = pluck(dfs_h2h_today, league_id, cur_matchup_period, competitor_id, window),
      "future" = pluck(dfs_h2h_future, league_id, cur_matchup_period, competitor_id, window),
    ),
    .id = "tense"
  ) |>
    mutate(selected_competitor = TRUE),

  bind_rows(
    lst(
      "past" = pluck(dfs_h2h_past, league_id, cur_matchup_period, opponent_id),
      "today" = pluck(dfs_h2h_today, league_id, cur_matchup_period, opponent_id, window),
      "future" = pluck(dfs_h2h_future, league_id, cur_matchup_period, opponent_id, window),
    ),
    .id = "tense"
  ) |>
    mutate(selected_competitor = FALSE)
)


# PLOT -------------------------------------------------------------------

plt_df <- df |>
  select(
    selected_competitor,
    player_id,
    player_name,
    inj_status,
    matches("f[g|t][m|a]"),
    any_of(unname(ls_lo_lg_cats[[league_id]][["Categories"]]))
  ) |>
  pivot_longer(
    c(matches("f[g|t][m|a]"), any_of(unname(ls_lo_lg_cats[[league_id]][["Categories"]])))
  ) |>
  mutate(
    value = if_else(inj_status == "Out", 0, value)
  ) |>
  summarise(
    value = sum(value, na.rm = TRUE),
    .by = c(selected_competitor, player_id, player_name, name)
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
          label = paste(str_c(player_name, " - ", round(!!m_col, 0), "/", round(!!a_col, 0)), collapse = "\n"),
          name = str_c(col_string, "_pct"),
          value = sum(!!m_col) / sum(!!a_col),
          .by = selected_competitor
        )
    }

    bind_rows(
      f_inr("fg"),
      f_inr("ft"),
      x |>
        filter(!str_like(name, "f[g|t][m|a]")) |>
        summarise(
          label = paste(str_c(player_name, " - ", round(value, 1)), collapse = "\n"),
          value = sum(value),
          .by = c(selected_competitor, name)
        )
    )
  })()


plotly::ggplotly(
  plt_df |>
    ggplot(aes(x = name, y = value, fill = selected_competitor, text = label)) +
    geom_col(position = "fill") +
    geom_hline(aes(yintercept = 0.5)) +
    # scale_y_continuous(labels = scales::label_percent()) +
    theme_bw() +
    labs(
      x = NULL,
      y = NULL
    ),
  tooltip = "text"
) |>
  layout(hovermode = "x") |>
  config(displayModeBar = FALSE)


# TABLE ------------------------------------------------------------------

df_tbl <- df |>
  arrange(game_date) |>
  select(selected_competitor, player_team, player_id, player_name, inj_status, fmt_date, scheduled_to_play) |>
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
  )


library(reactable)


col_fmt <- map(set_names(str_subset(colnames(df_tbl), "player_", negate = TRUE)), \(x) {
  colDef(minWidth = 70, style = function(value) {
    if (str_detect(value, "\\*")) {
      list(background = "#ea7878ff")
    }
  })
})
col_fmt[["selected_competitor"]] <- colDef(show = FALSE)
col_fmt[["player_id"]] <- colDef(show = FALSE)
col_fmt[["player_name"]] <- colDef(name = "Player", align = "left", minWidth = 140)
col_fmt[["player_team"]] <- colDef(name = "Team", align = "left", minWidth = 70)

rt_player <- reactable(
  filter(df_tbl, selected_competitor),
  pagination = FALSE,
  bordered = TRUE,
  highlight = TRUE,
  theme = reactableTheme(headerStyle = list(display = "none")),
  defaultSorted = list(player_team = "asc", player_name = "asc"),
  defaultColDef = colDef(headerStyle = list(background = "blue", color = "white")),
  columns = col_fmt,
)

# Table summary ----------------------------------------------------------

df_tbl_sum <- df_tbl |>
  summarise(
    across(contains("/"), \(x) {
      sum(as.numeric(x), na.rm = TRUE)
    }),
    .by = selected_competitor
  ) |>
  rename(player_team = selected_competitor) |>
  mutate(player_name = NA, .after = player_team)

rt_sum <- reactable(
  df_tbl_sum,
  pagination = FALSE,
  bordered = TRUE,
  highlight = TRUE,
  sortable = FALSE,
  defaultSorted = list(player_team = "asc", player_name = "asc"),
  defaultColDef = colDef(headerStyle = list(background = "blue", color = "white")),
  columns = col_fmt,
)

library(shiny)

shiny::tagList(
  tags$div(
    style = "overflow-x: auto; white-space: nowrap; padding: 5px;",

    tags$div(
      style = "width: 1200px;",
      rt_sum
    ),

    br(),

    tags$div(
      style = "width: 1200px;",
      rt_player
    )
  )
) |>
  htmltools::browsable()
