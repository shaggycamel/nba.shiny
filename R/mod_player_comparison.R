#' player_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList switchInput selectInput selectizeInput radioButtons sliderInput checkboxInput
#' @importFrom bslib layout_sidebar sidebar card
#' @importFrom reactable reactableOutput
mod_player_comparison_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        switchInput(
          ns("comparison_team_or_player"),
          value = TRUE,
          onLabel = "Team",
          offLabel = "Player",
          size = "small"
        ),
        selectInput(ns("comparison_team_or_player_filter"), NULL, choices = character(0), multiple = TRUE),
        selectizeInput(
          ns("comparison_excels_at_filter"),
          "Excels at",
          choices = character(0),
          options = list(maxItems = 5, onInitialize = I('function() { this.setValue(""); }'))
        ),
        radioButtons(ns("comparison_window"), "Rolling days", c(7, 15, 30), inline = TRUE),
        sliderInput(ns("comparison_minute_filter"), "Minute Filter", min = 0, max = 50, value = 20, round = TRUE),
        checkboxInput(ns("comparison_free_agent_filter"), "Free Agents only", value = TRUE),
      ),
      card(full_screen = TRUE, reactableOutput(ns("player_comparison_table"))),
      fillable = TRUE
    )
  )
}

#' player_comparison Server Functions
#'
#' @noRd
mod_player_comparison_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_player_comparison_ui("player_comparison_1")

## To be copied in the server
# mod_player_comparison_server("player_comparison_1")
