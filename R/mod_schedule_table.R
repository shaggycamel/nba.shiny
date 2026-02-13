#' schedule_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput dateInput radioButtons actionButton
#' @importFrom bslib layout_sidebar sidebar card
#' @importFrom reactable reactableOutput
mod_schedule_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        selectInput(ns("matchup_selection"), "Matchup", choices = character(0), selectize = FALSE),
        dateInput(ns("pin_date"), "Pinned Date"),
        radioButtons(ns("pin_dir"), label = "Pin Direction", choices = c("-", "+"), selected = "+", inline = TRUE),
        actionButton(ns("copy_teams"), "Copy teams to Comparison") # DOESN'T WORK
      ),
      card(full_screen = TRUE, reactableOutput(ns("schedule_table"))),
      fillable = TRUE
    ),
  )
}

#' schedule_table Server Functions
#'
#' @noRd
#'
#' @importFrom dplyr distinct filter between ungroup rowwise mutate c_across
#' @importFrom stringr str_extract
#' @importFrom shinyWidgets show_toast
#' @importFrom reactable reactable renderReactable colDef
#' @importFrom purrr pluck map
mod_schedule_table_server <- function(id, carry_thru) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # On load...
    observe({
      req(carry_thru()$fty_parameters_met())

      chs <- names(dfs_fty_nba_mup_weeks[[as.character(carry_thru()$selected$league_id)]])
      updateSelectInput(
        session = session,
        inputId = "matchup_selection",
        choices = chs,
        selected = chs[carry_thru()$selected$cur_matchup_period]
      )

      mup_min_max_dts <- dfs_fty_schedule[[as.character(carry_thru()$selected$league_id)]] |>
        distinct(matchup_period, matchup_start, matchup_end) |>
        filter(matchup_period == carry_thru()$selected$cur_matchup_period)

      updateDateInput(
        session,
        "pin_date",
        value = cur_date,
        min = mup_min_max_dts$matchup_start,
        max = mup_min_max_dts$matchup_end
      )
    }) |>
      bindEvent(carry_thru()$fty_parameters_met()) # Bind event of when league is swapped too

    # On matchup_selection change...
    observe({
      req(carry_thru()$fty_parameters_met())

      mup_min_max_dts <- dfs_fty_schedule[[as.character(carry_thru()$selected$league_id)]] |>
        filter(matchup_period == as.numeric(str_extract(input$matchup_selection, "^\\d+ "))) |>
        distinct(matchup_period, matchup_start, matchup_end)

      updateDateInput(
        session,
        "pin_date",
        value = if (between(as.Date(cur_date), mup_min_max_dts$matchup_start, mup_min_max_dts$matchup_end)) {
          cur_date
        } else if (as.Date(cur_date) > mup_min_max_dts$matchup_end) {
          mup_min_max_dts$matchup_end
        } else {
          mup_min_max_dts$matchup_start
        },
        min = mup_min_max_dts$matchup_start,
        max = mup_min_max_dts$matchup_end
      )
    }) |>
      bindEvent(input$matchup_selection, ignoreInit = TRUE)

    # On copy_teams...
    observe({
      req(carry_thru()$fty_parameters_met())
      # DOESN'T WORK
      # print(getReactableState("schedule_table", "selected"))
      # selected_values <- dfs_fty_nba_mup_weeks |>
      #   pluck(
      #     as.character(carry_thru()$selected$league_id),
      #     input$matchup_selection
      #   ) |>
      #   select(Team) |>
      #   slice(getReactableState("schedule_table", "sortedData"))
      # print(selected_values)

      # updateSwitchInput(session, "comparison_team_or_player", value = TRUE)
      # later::later(
      #   \() updateSelectInput(session, "comparison_team_or_player_filter", choices = teams, selected = tms),
      #   delay = 0.05
      # )
      show_toast(
        title = NULL,
        text = "Teams added to comparison...",
        position = "bottom-start",
        type = "info",
        timer = 2000
      )
    }) |>
      bindEvent(input$copy_teams, ignoreInit = TRUE)

    output$schedule_table <- renderReactable({
      req(carry_thru()$fty_parameters_met())

      # Pinned date calculations
      mup_dts <- reactive(
        dfs_fty_schedule[[as.character(carry_thru()$selected$league_id)]] |>
          filter(matchup_period == as.numeric(str_extract(input$matchup_selection, "^\\d+ "))) |>
          distinct(matchup_period, matchup_start, matchup_end)
      )

      pin_ix <- reactive(as.integer(difftime(input$pin_date, mup_dts()$matchup_start)))

      # dataframe for reactable
      df_tbl <- reactive({
        max_range <- as.integer(difftime(mup_dts()$matchup_end, mup_dts()$matchup_start))

        dfs_fty_nba_mup_weeks |>
          pluck(
            as.character(carry_thru()$selected$league_id),
            input$matchup_selection
          ) |>
          rowwise() |>
          mutate(
            Pin = sum(c_across(
              if (input$pin_dir == "+") {
                (pin_ix() + 2):(max_range + 3)
              } else {
                3:(pin_ix() + 2)
              }
            ))
          ) |>
          ungroup()
      })

      # Column formatting
      col_fmt <- map(set_names(tail(colnames(df_tbl()), 2)), ~ colDef(style = list(backgroundColor = "#eee5ff94")))
      col_fmt[[pluck(colnames(df_tbl()), pin_ix() + 3)]] <- colDef(style = list(backgroundColor = "#eaea78e8"))
      col_fmt[["Team"]] <- colDef(sticky = "left", style = list(backgroundColor = "#c1eccaff", fontWeight = "bold"))
      col_fmt[["Pin"]] <- colDef(
        sticky = "left",
        style = list(backgroundColor = "#c1eccaff", fontWeight = "bold", borderRight = "2px solid #0f0f0fff"),
        headerStyle = list(
          borderRight = "2px solid #0f0f0fff",
          background = "#0073b7",
          color = "white", # White text
          fontWeight = "bold",
          textAlign = "left",
          borderBottom = "2px solid #005a91"
        )
      )

      # get Team row values on click
      reactable(
        df_tbl(),
        defaultColDef = colDef(
          headerStyle = list(
            background = "#0073b7",
            color = "white", # White text
            fontWeight = "bold",
            textAlign = "left",
            borderBottom = "2px solid #005a91"
          )
        ),
        filterable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        pagination = FALSE,
        height = "85vh",
        wrap = TRUE,
        columns = col_fmt
      )
    })
  })
}

## To be copied in the UI
# mod_schedule_table_ui("schedule_table_1")

## To be copied in the server
# mod_schedule_table_server("schedule_table_1")

# library(shiny)
# library(bslib)
# library(reactable)
# library(stringr)
# library(purrr)
# library(dplyr)
# library(tidyr)
# library(shinyWidgets)
# load("data/dfs_fty_nba_mup_weeks.rda")
# load("data/dfs_fty_schedule.rda")
# load("data/cur_date.rda")

# ui <- page_fluid(
#   mod_schedule_table_ui("schedule_table_1")
# )

# server <- function(input, output, session) {
#   carry_thru <- reactiveVal(list(
#     fty_parameters_met = reactiveVal(TRUE),
#     selected = reactiveValues(
#       league_id = 24608,
#       cur_matchup_period = 17
#     )
#   ))

#   mod_schedule_table_server("schedule_table_1", carry_thru)
# }

# shinyApp(ui, server)
