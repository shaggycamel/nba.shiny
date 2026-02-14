#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #
  # ------- Base reactive
  carry_thru <- reactiveVal()

  #------- Login modal
  observe(carry_thru(mod_login_modal_server("login_modal_1"))) |>
    bindEvent(input$fty_league_competitor_switch, ignoreNULL = FALSE)

  # update dashboard title with selected league
  output$navbar_title <- renderUI({
    req(carry_thru()$selected$league_name)
    span(carry_thru()$selected$league_name)
  })

  #------- League Overview
  observe(mod_league_overview_server("league_overview_1", carry_thru)) |>
    bindEvent(carry_thru()$fty_parameters_met())

  #------- H2H

  #------- Schedule
  observe(mod_schedule_table_server("schedule_table_1", carry_thru)) |>
    bindEvent(carry_thru()$fty_parameters_met())

  #------- Player Comparison
  observe(mod_player_comparison_server("player_comparison_1", carry_thru)) |>
    bindEvent(carry_thru()$fty_parameters_met())
}
