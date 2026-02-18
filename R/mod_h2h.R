#' h2h UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput radioButtons checkboxInput actionButton
#' @importFrom bslib layout_sidebar sidebar layout_columns card
#' @importFrom plotly plotlyOutput
#' @importFrom reactable reactableOutput
mod_h2h_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        layout_columns(
          selectInput(ns("competitor"), "Competitor", choices = character(0)),
          selectInput(ns("matchup"), "Matchup", choices = 0)
        ),
        radioButtons(ns("window"), "Rolling days", c(7, 15, 30), inline = TRUE),
        layout_columns(
          selectInput(ns("ex_player"), "Exclude", choices = character(0), multiple = TRUE),
          selectInput(ns("add_player"), "Add", choices = character(0), multiple = TRUE),
        ),
        layout_columns(
          checkboxInput(ns("future_only"), "Future"),
          checkboxInput(ns("future_from_tomorrow"), "Tmrw")
        ),
        selectInput(ns("hl_player"), "Highlight Player", choices = character(0), multiple = TRUE),
        selectInput(ns("log_config"), "Log Filter Config", choices = character(0), size = 4, selectize = FALSE),
        actionButton(ns("snapshot_config"), "Snapshot config"),
      ),
      card(
        height = 1400,
        fill = FALSE,
        card(full_screen = TRUE, min_height = 500, max_height = 700, plotlyOutput(ns("stat_plot"))),
        card(full_screen = TRUE, min_height = 200, max_height = 650, reactableOutput(ns("game_table")))
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
        "competitor",
        choices = pluck(ls_fty_lookup, "cp_name_to_id", as.character(carry_thru()$selected$league_id)),
        selected = carry_thru()$selected$competitor_id
      )
      updateSelectInput(
        session,
        "matchup",
        choices = sort(unique((dfs_fty_schedule[[as.character(carry_thru()$selected$league_id)]])$matchup_period)),
        selected = carry_thru()$selected$cur_matchup_period
      )
      # updateSelectInput(session, "log_config", choices = ls_log_config)
    }) |>
      bindEvent(carry_thru()$fty_parameters_met())

    # Data prep --------------------------------------------------------------

    opponent_id <- reactive({
      pluck(dfs_fty_schedule, as.character(carry_thru()$selected$league_id)) |>
        filter(
          competitor_id == as.numeric(input$competitor),
          matchup_period == input$matchup
        ) |>
        pull(opponent_id)
    })

    df <- reactive({
      req(carry_thru()$fty_parameters_met())
    })

    #   # Game Table -------------------------------------------------------------

    output$game_table <- renderReactable({
      req(fty_parameters_met())

      reactable(df())
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
library(dplyr)
library(tidyr)
load("data/ls_fty_lookup.rda")
load("data/dfs_fty_schedule.rda")
load("data/dfs_h2h_past.rda")
load("data/dfs_h2h_today.rda")
load("data/dfs_h2h_future.rda")

ui <- page_fluid(
  mod_h2h_ui("h2h_1")
)

server <- function(input, output, session) {
  carry_thru <- reactiveVal(list(
    fty_parameters_met = reactiveVal(TRUE),
    selected = reactiveValues(
      platform = "ESPN",
      league_id = 1966813226,
      competitor_id = 5,
      competitor_name = "britney_spears",
      cur_matchup_period = 17
    )
  ))

  mod_h2h_server("h2h_1", carry_thru)
}

shinyApp(ui, server)
