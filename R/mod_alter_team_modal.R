#' alter_team_modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_alter_team_modal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(ns("blank_page"))
  )
}

#' alter_team_modal Server Functions
#'
#' @noRd
mod_alter_team_modal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Need to create reactive object that is passed between the modules
    observe({
      updateSelectInput(
        inputId = "player",
        choices = if (input$add_or_exclude) {
          # Free Agents
        } else {
          # Exisiting team..
        }
      )
    }) |>
      bindEvent(input$add_or_exclude, ignoreNULL = TRUE)

    showModal(
      modalDialog(
        # tags$head(tags$style(HTML(".selectize-dropdown-content{min-width: 100%; box-sizing: border-box;}"))),

        # Select Action
        switchInput(ns("add_or_exclude"), NULL, TRUE, "Add", "Exclude"),

        # Select Date
        dateInput(ns("action_date"), NULL),

        # Select Player
        selectInput(ns("player"), NULL, choices = character(0)),

        # Footer
        footer = tagList(
          actionButton(
            ns("dismiss"),
            label = NULL,
            icon = icon("square-xmark"),
            style = "color:#FFF; background-color:#CD3333; border-color:#2E6DA4"
          ),
          actionButton(
            ns("proceed"),
            "Kobeee!",
            style = "color:#FFF; background-color:#337AB7; border-color:#2E6DA4"
          )
        ),

        # Overall size
        size = "m"
      )
    )
  })
}

## To be copied in the UI
# mod_alter_team_modal_ui("alter_team_modal_1")

## To be copied in the server
# mod_alter_team_modal_server("alter_team_modal_1")

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
load("data/dfs_h2h_future.rda")

ui <- page_fluid(
  mod_alter_team_modal_ui("alter_team_modal_1")
)

server <- function(input, output, session) {
  carry_thru <- reactiveVal(list(
    fty_parameters_met = reactiveVal(TRUE),
    selected = reactiveValues(
      platform = "ESPN",
      league_id = 1382487116,
      competitor_id = 6,
      cur_matchup_period = 21
    )
  ))

  mod_alter_team_modal_server("alter_team_modal_1")
}

shinyApp(ui, server)
