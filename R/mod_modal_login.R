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
mod_modal_login_server <- function(id, rv_carry_thru, rv_switch_league_counter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      if (input$fty_league_select != "" & input$fty_competitor_select != "") {
        rv_carry_thru$league_name <- input$fty_league_select
        rv_carry_thru$league_id <- pluck(ls_fty_lookup, "lg_name_to_id", rv_carry_thru$league_name)
        rv_carry_thru$platform <- pluck(ls_fty_lookup, "lg_id_to_platform", as.character(rv_carry_thru$league_id))
        rv_carry_thru$cur_matchup_period <- pluck(dfs_fty_schedule, as.character(rv_carry_thru$league_id)) |>
          filter(matchup_start <= cur_date, matchup_end >= cur_date) |>
          pull(matchup_period) |>
          pluck(1)
        rv_carry_thru$competitor_name <- input$fty_competitor_select
        rv_carry_thru$competitor_id <- pluck(
          ls_fty_lookup,
          "cp_name_to_id",
          as.character(rv_carry_thru$league_id),
          rv_carry_thru$competitor_name
        )
        rv_carry_thru$fty_parameters_met <- TRUE
        removeModal()
        output$login_messages <- NULL
      } else if (input$fty_league_select != "") {
        output$login_messages <- renderText("Select a competitor...")
      } else {
        output$login_messages <- renderText("Select a league...")
      }
    }) |>
      bindEvent(input$fty_dash_init, ignoreInit = TRUE)

    # Gotta fill the form at least once!
    observe({
      if (!!rv_carry_thru$fty_parameters_met) {
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
    }) |>
      bindEvent(rv_switch_league_counter(), ignoreNULL = FALSE, ignoreInit = FALSE)
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
