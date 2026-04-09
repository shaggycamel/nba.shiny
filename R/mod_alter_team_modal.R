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
mod_alter_team_modal_server <- function(id, rv_alter_team, rv_alter_team_modal_vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    print(isolate(rv_alter_team_modal_vals))

    # Modal Scaffolding ------------------------------------------------------

    # Need to create reactive object that is passed between the modules

    showModal(
      modalDialog(
        # tags$head(tags$style(HTML(".selectize-dropdown-content{min-width: 100%; box-sizing: border-box;}"))),

        # Select Action
        switchInput(ns("add_or_exclude"), NULL, TRUE, "Add", "Exclude"),

        # Select Date
        dateInput(
          ns("action_date"),
          label = NULL,
          value = isolate(rv_alter_team_modal_vals)$ui_date,
          min = isolate(rv_alter_team_modal_vals)$ui_date,
          max = isolate(rv_alter_team_modal_vals)$mup_end_date,
          weekstart = 1
        ),

        # Select Player
        selectInput(ns("player"), NULL, choices = character(0)),

        # Footer
        footer = tagList(
          span(textOutput(ns("message")), style = "color:red"), # Align left somehow
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

    # Reactive player list ---------------------------------------------------

    observe({
      updateSelectInput(
        inputId = "player",
        choices = if (input$add_or_exclude) {
          isolate(rv_alter_team_modal_vals)$free_agents
        } else {
          isolate(rv_alter_team_modal_vals)$roster
        },
        selected = NA
      )
    }) |>
      bindEvent(input$add_or_exclude, ignoreNULL = TRUE)

    # Proceed button ---------------------------------------------------------

    observe({
      if (input$player != "") {
        rv_alter_team[[paste0(
          if (input$add_or_exclude) "add" else "ex",
          "-",
          input$player
        )]] = lst(
          "add_or_exclude" = if (input$add_or_exclude) "add" else "ex",
          "action_date" = input$action_date,
          "player_id" = input$player,
          "player_name" = if (input$add_or_exclude) {
            names(keep(rv_alter_team_modal_vals()$free_agents, \(x) x == input$player))
          } else {
            names(keep(rv_alter_team_modal_vals()$roster, \(x) x == input$player))
          }
        )
        # print(rv_alter_team)
        removeModal()
        output$message <- NULL
      } else {
        output$message <- renderText("Select a player...")
      }
    }) |>
      bindEvent(input$proceed)

    # Dismiss button ---------------------------------------------------------

    observe(removeModal()) |>
      bindEvent(input$dismiss)
  })
}

## To be copied in the UI
# mod_alter_team_modal_ui("alter_team_modal_1")

## To be copied in the server
# mod_alter_team_modal_server("alter_team_modal_1")

# library(shiny)
# library(bslib)
# library(shinyWidgets)
# library(reactable)
# library(plotly)
# library(stringr)
# library(purrr)
# library(tibble)
# library(dplyr)
# library(tidyr)
# library(scales)
# library(lubridate)

# load("data/cur_date.rda")
# load("data/dfs_h2h_future.rda")

# ui <- page_fluid(
#   mod_alter_team_modal_ui("alter_team_modal_1")
# )

# server <- function(input, output, session) {
#   rv_alter_team = reactiveValues()
#   rv_alter_team_modal_vals <- reactiveVal(list(
#     roster = NULL,
#     free_agents = NULL,
#     ui_date = Sys.Date(),
#     mup_end_date = Sys.Date()
#   ))

#   mod_alter_team_modal_server(
#     "alter_team_modal_1",
#     rv_alter_team,
#     rv_alter_team_modal_vals()
#   )
# }

# shinyApp(ui, server)
