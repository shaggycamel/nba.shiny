#' h2h UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_h2h_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        layout_columns(
          selectInput(ns("h2h_competitor"), "Competitor", choices = character(0)),
          selectInput(ns("h2h_matchup"), "Matchup", choices = 0)
        ),
        radioButtons(ns("h2h_window"), "Rolling days", c(7, 15, 30), inline = TRUE),
        layout_columns(
          selectInput(ns("h2h_ex_player"), "Exclude", choices = character(0), multiple = TRUE),
          selectInput(ns("h2h_add_player"), "Add", choices = character(0), multiple = TRUE),
        ),
        layout_columns(
          checkboxInput(ns("h2h_future_only"), "Future"),
          checkboxInput(ns("h2h_future_from_tomorrow"), "Tmrw")
        ),
        selectInput(ns("h2h_hl_player"), "Highlight Player", choices = character(0), multiple = TRUE),
        selectInput(ns("h2h_log_config"), "Log Filter Config", choices = character(0), size = 4, selectize = FALSE),
        actionButton(ns("h2h_snapshot_config"), "Snapshot config"),
      ),
      card(
        height = 1400,
        fill = FALSE,
        card(full_screen = TRUE, min_height = 500, max_height = 700, plotlyOutput(ns("h2h_stat_plot"))),
        card(full_screen = TRUE, min_height = 200, max_height = 650, DTOutput(ns("h2h_game_table")))
      ),
      fillable = TRUE,
      tags$style(
        type = "text/css",
        ".selectize-dropdown-content{width: 200px;background-color: #FFFFFF; align: right;}"
      )
    )
  )
}


#' h2h Server Functions
#'
#' @noRd
mod_h2h_server <- function(id, carry_thru) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update UI --------------------------------------------------------------

    observe({
      req(carry_thru()$fty_parameters_met())

      updateSelectInput(
        session,
        "h2h_competitor",
        choices = pluck(ls_fty_lookup, "cp_name_to_id", as.character(carry_thru()$selected$league_id)),
        selected = carry_thru()$selected$competitor_id
      )
      updateSelectInput(
        session,
        "h2h_matchup",
        choices = sort(unique((dfs_fty_schedule[[as.character(carry_thru()$selected$league_id)]])$matchup_period)),
        selected = carry_thru()$selected$cur_matchup_period
      )
      # updateSelectInput(session, "h2h_log_config", choices = ls_log_config)
    }) |>
      bindEvent(carry_thru()$fty_parameters_met())

    # Data prep --------------------------------------------------------------

    # df <- reactive({
    # pluck(dfs_h2h_past, as.character(carry_thru()$selected$league_id))
    # pluck(df_rolling_stats, input$h2h_window)
    # })
    add <- NULL # DELETE
    df_h2h_today <- pluck(dfs_fty_roster, "95537") |>
      filter(assigned_date == cur_date - ddays(3)) |>
      mutate(dow = lubridate::wday(assigned_date, week_start = 1), .after = assigned_date) |>
      left_join(
        select(df_nba_schedule, team, game_date, scheduled_to_play),
        by = join_by(player_team == team, assigned_date == game_date)
      ) |>
      rename(game_date = assigned_date) |>
      bind_rows(
        filter(df_player_box_score, player_name %in% add) |>
          slice_max(order_by = game_date, by = player_name) |>
          select(
            player_id,
            # player_fantasy_id := str_c(str_to_lower(carry_thru()$selected$platform), "_id"),
            player_name,
            player_team = team_slug
          ) |>
          mutate(season = cur_season, competitor_id = 25)
        # mutate(season = cur_season, competitor_id = carry_thru()$selected$competitor_id)
      ) |>
      select(-c(season_type, begin_date, end_date))

    df_h2h_future <- distinct(df_nba_schedule, game_date) |>
      filter(game_date > cur_date) |>
      # cross_join(tibble(
      #   competitor_id = unlist(
      #     pluck(ls_fty_lookup, "cp_name_to_id", as.character(carry_thru()$selected$league_id)),
      #     use.names = FALSE
      #   )
      # )) |>
      cross_join(tibble(competitor_id = unlist(pluck(ls_fty_lookup, "cp_name_to_id", "95537"), use.names = FALSE))) |>
      left_join(
        select(
          df_h2h_today,
          season,
          platform,
          # league_id,
          competitor_id,
          player_fantasy_id,
          player_id,
          player_name,
          player_team,
          player_injury_status
        ),
        by = join_by(competitor_id),
        relationship = "many-to-many"
      ) |>
      left_join(
        select(df_nba_schedule, game_date, team, scheduled_to_play),
        by = join_by(game_date, player_team == team)
      ) |>
      left_join(
        pluck(dfs_fty_schedule, "95537") |>
          select(starts_with("matchup"), competitor_id, opponent_id),
        by = join_by(competitor_id, between(game_date, matchup_start, matchup_end))
      ) |>
      mutate(dow = lubridate::wday(game_date, week_start = 1)) |>
      select(any_of(colnames(pluck(dfs_h2h_past$data, 1))))

    # COMBINE
    df_h2h <- bind_rows(
      list(past = pluck(dfs_h2h_past, "95537"), today = df_h2h_today, future = df_h2h_future),
      .id = "origin"
    ) |>
      left_join(
        select(pluck(df_rolling_stats, "7"), -c(espn_id, yahoo_id, player_name, team_slug)),
        by = join_by(player_id, game_date)
      )

    # FROM TOMORROW TWEAKING
    if (from_tomorrow) {
      df_h2h <- df_h2h |>
        anti_join(
          filter(df_h2h, competitor_id == c_id, player_name %in% add, origin == "today"),
          by = join_by(competitor_id, player_id, game_date)
        ) |>
        anti_join(
          filter(df_h2h, competitor_id == c_id, player_name %in% exclude, origin == "future"),
          by = join_by(competitor_id, player_id, game_date)
        )
    } else {
      df_h2h <- df_h2h |>
        anti_join(
          filter(df_h2h, competitor_id == c_id, player_name %in% exclude, origin != "past"),
          by = join_by(competitor_id, player_id, game_date)
        )
    }
  })
}

## To be copied in the UI
# mod_h2h_ui("h2h_1")

## To be copied in the server
# mod_h2h_server("h2h_1")

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(plotly)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
load("data/ls_fty_lookup.rda")
load("data/dfs_fty_schedule.rda")

ui <- page_fluid(
  mod_h2h_ui("h2h_1")
)

server <- function(input, output, session) {
  carry_thru <- reactiveVal(list(
    fty_parameters_met = reactiveVal(TRUE),
    selected = reactiveValues(
      platform = "ESPN",
      league_id = 95537,
      competitor_id = 25,
      competitor_name = "britney_spears",
      cur_matchup_period = 13
    )
  ))

  mod_h2h_server("h2h_1", carry_thru)
}

shinyApp(ui, server)
