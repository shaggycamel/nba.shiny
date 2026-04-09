#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #
  # ------- Base reactive
  rv_carry_thru <- reactiveVal()
  rv_alter_team <- reactiveValues()
  rv_alter_team_modal_vals <- reactiveVal()
  rv_copy_teams <- reactiveVal(NULL)
  # MAYBE MOVE INITIALISATION OF fty_pararmeters_met HERE....

  #------- Login modal
  observe(rv_carry_thru(mod_login_modal_server("login_modal_1"))) |>
    bindEvent(input$fty_league_competitor_switch, ignoreNULL = FALSE)

  # update dashboard title with selected league and player
  output$navbar_title <- renderUI({
    req(rv_carry_thru()$fty_parameters_met())
    span(rv_carry_thru()$selected$league_name, " - ", rv_carry_thru()$selected$competitor_name)
  })

  #------- League Overview
  mod_league_overview_server("league_overview_1", rv_carry_thru)

  #------- H2H
  mod_h2h_server("h2h_1", rv_carry_thru, rv_alter_team, rv_alter_team_modal_vals())

  # #------- H2H: Alter team modal
  # mod_alter_team_modal_server("alter_team_modal_1", rv_alter_team, rv_alter_team_modal_vals())

  #------- Schedule
  mod_schedule_table_server("schedule_table_1", rv_carry_thru, rv_copy_teams)

  #------- Player Comparison
  mod_player_comparison_server("player_comparison_1", rv_carry_thru, rv_copy_teams)
}
