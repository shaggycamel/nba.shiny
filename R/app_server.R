#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #
  # ------- Base reactives
  rv_carry_thru <- reactiveVal()
  rv_alter_team <- reactiveValues()
  rv_alter_team_modal_vals <- reactiveVal()
  rv_alter_team_counter <- reactiveVal(0L)
  rv_copy_teams <- reactiveVal(NULL)

  #------- Login modal
  observe(rv_carry_thru(mod_modal_login_server("modal_login_1"))) |>
    bindEvent(input$fty_league_competitor_switch, ignoreNULL = FALSE)

  # Update dashboard title with selected league and player
  output$navbar_title <- renderUI({
    req(rv_carry_thru()$fty_parameters_met())
    span(rv_carry_thru()$selected$league_name, " - ", rv_carry_thru()$selected$competitor_name)
  })

  #------- League Overview
  mod_league_overview_server("league_overview_1", rv_carry_thru)

  #------- H2H
  mod_h2h_server("h2h_1", rv_carry_thru, rv_alter_team, rv_alter_team_modal_vals, rv_alter_team_counter)

  #------- H2H: Alter team modal
  mod_modal_alter_team_server("modal_alter_team_1", rv_alter_team, rv_alter_team_modal_vals, rv_alter_team_counter)

  #------- Schedule
  mod_schedule_table_server("schedule_table_1", rv_carry_thru, rv_copy_teams)

  #------- Player Comparison
  mod_player_comparison_server("player_comparison_1", rv_carry_thru, rv_copy_teams)
}
