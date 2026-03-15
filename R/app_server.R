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
  copy_teams_trigger <- reactiveVal(NULL)

  #------- Login modal
  observe(carry_thru(mod_login_modal_server("login_modal_1"))) |>
    bindEvent(input$fty_league_competitor_switch, ignoreNULL = FALSE)

  # update dashboard title with selected league
  output$navbar_title <- renderUI({
    req(carry_thru()$selected$league_name)
    span(carry_thru()$selected$league_name)
  })

  #------- League Overview
  mod_league_overview_server("league_overview_1", carry_thru)

  #------- H2H
  mod_h2h_server("h2h_1", carry_thru)

  #------- Schedule
  mod_schedule_table_server("schedule_table_1", carry_thru, copy_teams_trigger)

  #------- Player Comparison
  mod_player_comparison_server("player_comparison_1", carry_thru, copy_teams_trigger)
}
