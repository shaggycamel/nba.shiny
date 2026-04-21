#' h2h UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_h2h_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        layout_columns(
          selectInput(ns("matchup"), NULL, choices = 0),
          checkboxInput(ns("future_only"), "Future"),
        ),
        radioButtons(ns("window"), "Rolling days", c(7, 15, 30), inline = TRUE),
        dateInput(ns("pin_date"), NULL, weekstart = 1),
        selectInput(ns("hl_player"), "Highlight Player", choices = character(0), multiple = TRUE),
        actionButton(ns("alter_team"), "Alter Team"),
        reactableOutput(ns("alter_team_table")),
        # actionButton(ns("snapshot_config"), "Snapshot config"),
      ),
      card(
        height = 1400,
        fill = FALSE,
        card(full_screen = TRUE, min_height = 500, max_height = 700, plotlyOutput(ns("stat_plot"))),
        card(
          full_screen = TRUE,
          min_height = 200,
          max_height = 650,
          reactableOutput(ns("game_table"))
        )
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
#'
mod_h2h_server <- function(id, rv_carry_thru, rv_alter_team, rv_alter_team_modal_vals, rv_alter_team_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update UI --------------------------------------------------------------

    observe({
      req(rv_carry_thru()$fty_parameters_met())

      updateSelectInput(
        session,
        "matchup",
        choices = sort(unique(
          (pluck(dfs_fty_schedule, as.character(rv_carry_thru()$selected$league_id)))$matchup_period
        )),
        selected = rv_carry_thru()$selected$cur_matchup_period
      )

      updateSelectInput(
        session,
        "hl_player",
        choices = pluck(dfs_fty_roster, as.character(rv_carry_thru()$selected$league_id)) |>
          filter(competitor_id == rv_carry_thru()$selected$competitor_id) |>
          slice_max(assigned_date) |>
          arrange(player_name) |>
          select(player_name, player_id) |>
          na.omit() |>
          deframe()
      )

      # updateSelectInput(session, "log_config", choices = ls_log_config)
    }) |>
      bindEvent(rv_carry_thru()$fty_parameters_met())

    # Initial date picker, relies on df_base
    observe({
      req(df_base())

      updateDateInput(
        session,
        "pin_date",
        value = rv_carry_thru()$selected$ui_date,
        min = unique(na.omit(df_base()$matchup_start)),
        max = unique(na.omit(df_base()$matchup_end))
      )
    }) |>
      bindEvent(df_base(), ignoreInit = FALSE)

    # Ongoing Date picker pin_date
    observe({
      req(nrow(df_base()) > 0)

      updateDateInput(
        session,
        "pin_date",
        value = if (max(na.omit(df_base()$matchup_end)) <= rv_carry_thru()$selected$ui_date) {
          max(na.omit(df_base()$matchup_end))
        } else {
          min(na.omit(df_base()$matchup_start))
        },
        min = unique(na.omit(df_base()$matchup_start)),
        max = unique(na.omit(df_base()$matchup_end))
      )
    }) |>
      bindEvent(input$matchup, ignoreInit = TRUE)

    # observe({
    #   req(df_base())
    #   players_already_hl <- setdiff(input$hl_player, input$ex_player)

    #   updateSelectInput(
    #     session,
    #     "hl_player",
    #     choices = df_base() |>
    #       filter(
    #         competitor ==
    #           pluck(ls_fty_lookup, "cp_id_to_name", as.character(rv_carry_thru()$selected$league_id), input$competitor)
    #       ) |>
    #       arrange(player_name) |>
    #       select(player_name, player_id) |>
    #       na.omit() |>
    #       deframe(),
    #     selected = players_already_hl
    #   )
    # }) |>
    #   bindEvent(input$add_player, input$ex_player)

    observe({
      req(opponent())

      # Update modal data
      rv_alter_team_modal_vals$roster <- df_base() |>
        filter(competitor_id == as.integer(rv_carry_thru()$selected$competitor_id)) |>
        distinct(player_name, player_id) |>
        arrange(player_name) |>
        deframe()

      rv_alter_team_modal_vals$free_agents <- dfs_h2h_future |>
        pluck(as.character(rv_carry_thru()$selected$league_id), "free_agent", "free_agent", "7") |>
        distinct(player_name, player_id) |>
        anti_join(
          df_base() |>
            filter(competitor_id == as.integer(rv_carry_thru()$selected$competitor_id)) |>
            distinct(player_name, player_id),
          by = join_by(player_id)
        ) |>
        na.omit() |>
        deframe()

      rv_alter_team_modal_vals$ui_date <- input$pin_date
      rv_alter_team_modal_vals$mup_end_date = unique(df_base()$matchup_end)

      rv_alter_team_trigger(isolate(rv_alter_team_trigger()) + 1L)
    }) |>
      bindEvent(input$alter_team, ignoreInit = TRUE)

    # Alter Team Table -------------------------------------------------------

    output$alter_team_table <- renderReactable({
      selected_players <- if (length(rv_alter_team) > 0) names(rv_alter_team) else NA_character_

      df <- tibble(
        key = selected_players,
        action = str_split_i(selected_players, "-", 1),
        player_name = str_split_i(selected_players, "-", 2),
        delete = na_lgl
      )

      reactable(
        df,
        pagination = FALSE,
        sortable = FALSE,
        compact = TRUE,
        columns = list(
          key = colDef(show = FALSE),
          action = colDef(name = "Action", maxWidth = 70),
          player_name = colDef(name = "Player", minWidth = 120),
          delete = colDef(
            name = "",
            maxWidth = 30,
            sortable = FALSE,
            cell = function(value, index) {
              tags$button(
                icon("xmark"),
                class = "btn btn-danger btn-sm",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                  ns("delete_alter_team_key"),
                  df$key[index]
                )
              )
            }
          )
        )
      )
    })

    # observeEvent(input$delete_alter_team_key, {
    #   rv_alter_team[[input$delete_alter_team_key]] <- NULL
    # })

    # Data prep --------------------------------------------------------------

    opponent <- reactive({
      req(input$matchup > 0)
      get_opponent(rv_carry_thru, as.numeric(input$matchup))
    }) |>
      bindEvent(input$matchup)

    df_base <- reactive({
      req(opponent())
      print("base updating...")
      base_data_prep(input, rv_carry_thru()$selected, opponent, rv_alter_team)
    }) |>
      bindEvent(opponent(), reactiveValuesToList(rv_alter_team), ignoreInit = FALSE)

    df_plt <- reactive({
      req(nrow(df_base()) > 0)
      plot_data_prep(df_base(), rv_carry_thru()$selected)
    })

    # Potentially turn this into a static df - haven't thought it thru yet
    # Haven't tested on whehter players can be added/excluded and still greyed out...
    df_grey_player <- reactive({
      req(nrow(df_base()) > 0)
      grey_player_data_prep(input, df_base(), rv_carry_thru()$selected, opponent)
    })

    df_tbl <- reactive({
      req(nrow(df_base()) > 0, df_grey_player(), pin_ix())
      table_data_prep(df_base(), rv_carry_thru()$selected, df_grey_player(), pin_ix())
    })

    df_tbl_sum <- reactive({
      req(df_tbl())
      table_sum_data_prep(df_tbl(), df_base(), pin_ix())
    })

    pin_ix <- reactive({
      req(nrow(df_base()) > 0)

      df_base() |>
        distinct(game_date) |>
        na.omit() |>
        pull(game_date) |>
        sort() |>
        detect_index(\(x) x == as.Date(input$pin_date))
    }) |>
      bindEvent(input$pin_date)

    # Plot -------------------------------------------------------------------

    output$stat_plot <- renderPlotly({
      req(df_plt())

      ggplotly(
        df_plt() |>
          ggplot(aes(x = name, y = value, fill = competitor, text = label)) +
          geom_col(position = "fill") +
          geom_hline(aes(yintercept = 0.5)) +
          # scale_y_continuous(labels = scales::label_percent()) +
          scale_fill_brewer(type = "qual", palette = "Set2", direction = -1) +
          theme_bw() +
          labs(x = NULL, y = NULL),
        tooltip = "text"
      ) |>
        layout(hovermode = "x") |>
        config(displayModeBar = FALSE)
    })

    # Game Table -------------------------------------------------------------

    # SOMEWHERE THE DATA BEING FED INTO THIS BECOMES A LSIT
    output$game_table <- renderReactable({
      req(df_tbl_sum(), df_tbl(), df_grey_player())

      col_fmt <- game_tbl_col_fmt(df_tbl(), input$pin_date, unique(na.omit(df_base()$matchup_end)))
      col_fmt_sum <- game_tbl_col_fmt(df_tbl_sum(), input$pin_date, unique(na.omit(df_base()$matchup_end)), "sum")

      reactable(
        df_tbl_sum(),
        pagination = FALSE,
        bordered = TRUE,
        style = list(border = "1px solid #000000"),
        rowStyle = list(borderBottom = "1px solid #000000"),
        theme = reactableTheme(headerStyle = list(borderBottom = "1px solid #000000")),
        highlight = TRUE,
        sortable = FALSE,
        defaultColDef = colDef(headerStyle = list(background = "#cce5ff")),
        columns = col_fmt_sum,
        defaultExpanded = TRUE,
        details = \(ix) {
          tags$div(
            style = "margin-left: 45px; margin-top: 10px; margin-bottom: 30px;",
            reactable(
              filter(df_tbl(), competitor == df_tbl_sum()$player_team[ix]),
              pagination = FALSE,
              bordered = TRUE,
              style = list(border = "1px solid #000000"),
              highlight = TRUE,
              theme = reactableTheme(headerStyle = list(display = "none")),
              defaultSorted = list(player_team = "asc", player_name = "asc"),
              defaultColDef = colDef(headerStyle = list(background = "#cce5ff")), # Not sure if needed
              columns = col_fmt,
              rowStyle = function(index) {
                if (df_tbl()$player_id[index] %in% as.numeric(input$hl_player)) {
                  list(backgroundColor = "#ffef9dff", fontWeight = "bold") # Light yellow highlight
                }
              },
            )
          )
        }
      )
    })
  })
}

## To be copied in the UI
# mod_h2h_ui("h2h_1")

## To be copied in the server
# mod_h2h_server("h2h_1")

library(shiny)
library(bslib)
library(shinyWidgets)
library(reactable)
library(plotly)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)

load("data/cur_date.rda")
load("data/ls_fty_lookup.rda")
load("data/ls_lo_lg_cats.rda")
load("data/dfs_fty_schedule.rda")
load("data/dfs_fty_roster.rda")
load("data/dfs_h2h_past.rda")
load("data/dfs_h2h_future.rda")

source("R/mod_h2h_fct_base_data_prep.R")
source("R/mod_h2h_fct_plot_data_prep.R")
source("R/mod_h2h_fct_table_data_prep.R")
source("R/mod_h2h_fct_game_tbl_col_fmt.R")
source("R/utils_get_opponent.R")
source("R/mod_modal_alter_team.R")
source("R/mod_h2h_fct_base_data_prep.R")

ui <- page_fluid(
  mod_h2h_ui("h2h_1")
)

server <- function(input, output, session) {
  rv_carry_thru <- reactiveVal(list(
    fty_parameters_met = reactiveVal(TRUE),
    selected = reactiveValues(
      platform = "ESPN",
      league_id = 1382487116,
      competitor_id = 6,
      cur_matchup_period = 22,
      ui_date = as.Date("2026-03-29")
    )
  ))
  rv_alter_team <- reactiveValues()
  rv_alter_team_modal_vals <- reactiveValues()
  rv_alter_team_trigger <- reactiveVal(0L)

  mod_h2h_server("h2h_1", rv_carry_thru, rv_alter_team, rv_alter_team_modal_vals, rv_alter_team_trigger)
  mod_modal_alter_team_server("modal_alter_team_1", rv_alter_team, rv_alter_team_modal_vals, rv_alter_team_trigger)
}

shinyApp(ui, server)
