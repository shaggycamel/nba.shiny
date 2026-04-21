#' login_modal Server Functions
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_modal_login_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidPage(ns("blank_page"))
  )
}

#' @noRd
#'
mod_modal_login_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for selections and met_parameters boolean
    fty_parameters_met <- reactiveVal(FALSE)
    selected <- reactiveValues()

    # Main code to assign values to reactive and close modal when complete
    observe({
      if (input$fty_league_select != "" & input$fty_competitor_select != "") {
        selected$league_name <- input$fty_league_select
        selected$league_id <- pluck(ls_fty_lookup, "lg_name_to_id", selected$league_name)
        selected$platform <- pluck(ls_fty_lookup, "lg_id_to_platform", as.character(selected$league_id))
        selected$ui_date <- min(
          max(pull(pluck(dfs_fty_schedule, as.character(selected$league_id)), matchup_end)),
          cur_date
        )
        selected$cur_matchup_period <- pluck(dfs_fty_schedule, as.character(selected$league_id)) |>
          filter(matchup_start <= selected$ui_date, matchup_end >= selected$ui_date) |>
          pull(matchup_period) |>
          pluck(1)
        selected$competitor_name <- input$fty_competitor_select
        selected$competitor_id <- pluck(
          ls_fty_lookup,
          "cp_name_to_id",
          as.character(selected$league_id),
          selected$competitor_name
        )
        fty_parameters_met(TRUE)
        removeModal()
        output$login_messages <- NULL
      } else if (input$fty_league_select != "" & input$fty_competitor_select == "") {
        output$login_messages <- renderText("Select a competitor...")
      } else {
        output$login_messages <- renderText("Select a league...")
      }
    }) |>
      bindEvent(input$fty_dash_init, ignoreInit = TRUE)

    # Gotta fill the form at least once!
    observe({
      if (!fty_parameters_met()) {
        output$login_messages <- renderText("You gotta go fill the form at least once!")
      } else {
        removeModal()
        output$login_messages <- NULL
      }
    }) |>
      bindEvent(input$fty_abort)

    # Make competitor list update based on league selected
    observe({
      updateSelectInput(
        inputId = "fty_competitor_select",
        choices = filter(df_fty_base, league_name == input$fty_league_select) |>
          pull(competitor_name)
      )
    }) |>
      bindEvent(input$fty_league_select, ignoreNULL = TRUE)

    # Modal UI structure.
    # # This needs to be defined in the server component.
    observe({
      showModal(
        modalDialog(
          tags$head(tags$style(HTML(".selectize-dropdown-content{min-width: 100%; box-sizing: border-box;}"))),

          # Select League
          selectizeInput(
            ns("fty_league_select"),
            label = NULL,
            choices = unique(df_fty_base$league_name),
            options = list(
              placeholder = "Select Fantasy League",
              onInitialize = I("function(){this.setValue('');}")
            ),
            width = "100%"
          ),

          # Select Competitor
          selectizeInput(
            ns("fty_competitor_select"),
            label = NULL,
            choices = character(0),
            options = list(
              placeholder = "Select Fantasy Competitor",
              onInitialize = I("function(){this.setValue('');}")
            ),
            width = "100%"
          ),

          span(textOutput(ns("login_messages")), style = "color:red"),
          footer = tagList(
            actionButton(
              ns("fty_abort"),
              label = NULL,
              icon = icon("square-xmark"),
              style = "color:#FFF; background-color:#CD3333; border-color:#2E6DA4"
            ),
            actionButton(
              ns("fty_dash_init"),
              "Kobeee!",
              style = "color:#FFF; background-color:#337AB7; border-color:#2E6DA4"
            )
          ),
          size = "m"
        )
      )

      # Return list containing necessary elements
      lst(
        fty_parameters_met,
        selected
      )
    }) |>
      bindEvent(input$fty_league_competitor_switch, ignoreNULL = FALSE, ignoreInit = FALSE)
  })
}

## To be copied in the server
# mod_modal_login_server("modal_login_1")

# library(shiny)
# library(bslib)
# library(dplyr)
# load("data/df_fty_base.rda")
# load("data/ls_fty_lookup.rda")

# ui <- page_fluid(
#   mod_modal_login_ui("modal_login_1")
# )

# server <- function(input, output, session) {
#   mod_modal_login_server("modal_login_1")
# }

# shinyApp(ui, server)
