#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #
  # ------- Base reactives
  rv_carry_thru <- reactiveValues(fty_parameters_met = FALSE)
  rv_alter_team <- reactiveVal(list())
  rv_alter_team_modal_vals <- reactiveValues()
  rv_alter_team_trigger <- reactiveVal(0L)
  rv_snapshot_log <- reactiveVal(list())
  rv_snapshot_trigger <- reactiveVal(0L)
  rv_copy_teams <- reactiveVal(NULL)
  rv_switch_league_trigger <- reactiveVal(0L)

  #------- Login modal
  mod_modal_login_server(
    "modal_login_1",
    rv_carry_thru,
    rv_switch_league_trigger,
    rv_alter_team,
    rv_snapshot_log,
    rv_snapshot_trigger
  )
  observe(rv_switch_league_trigger(isolate(rv_switch_league_trigger()) + 1L)) |>
    bindEvent(input$fty_league_competitor_switch)

  # Update dashboard title with selected league and player
  output$navbar_title <- renderUI({
    req(rv_carry_thru$fty_parameters_met)
    span(rv_carry_thru$league_name, " - ", rv_carry_thru$competitor_name)
  })

  #------- League Overview
  mod_league_overview_server("league_overview_1", rv_carry_thru)

  #------- H2H
  mod_h2h_server(
    "h2h_1",
    rv_carry_thru,
    rv_alter_team,
    rv_alter_team_modal_vals,
    rv_alter_team_trigger,
    rv_snapshot_log,
    rv_snapshot_trigger
  )

  #------- H2H: Alter team modal
  mod_modal_alter_team_server("modal_alter_team_1", rv_alter_team, rv_alter_team_modal_vals, rv_alter_team_trigger)

  #------- Schedule
  mod_schedule_table_server("schedule_table_1", rv_carry_thru, rv_copy_teams)

  #------- Player Comparison
  mod_player_comparison_server("player_comparison_1", rv_carry_thru, rv_copy_teams)
}
