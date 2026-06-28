#' schedule_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_schedule_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        selectInput(ns("matchup_selection"), "Matchup", choices = character(0), selectize = FALSE),
        dateInput(ns("pin_date"), "Pinned Date"),
        radioButtons(ns("pin_dir"), label = "Pin Direction", choices = c("-", "+"), selected = "+", inline = TRUE),
        actionButton(ns("copy_teams"), "Copy teams to Comparison")
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
mod_schedule_table_server <- function(id, rv_carry_thru, rv_copy_teams) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialise & Filter Reactivity --------------------------------------------------------

    # On load...
    observe({
      req(rv_carry_thru$fty_parameters_met)

      chs <- names(pluck(dfs_fty_nba_mup_weeks, as.character(rv_carry_thru$league_id)))
      updateSelectInput(
        session = session,
        inputId = "matchup_selection",
        choices = chs,
        selected = if (rv_carry_thru$cur_matchup_period == 99) tail(chs, 1) else chs[rv_carry_thru$cur_matchup_period]
      )

      mup_min_max_dts <- pluck(dfs_fty_schedule, as.character(rv_carry_thru$league_id)) |>
        distinct(matchup_period, matchup_start, matchup_end) |>
        filter(matchup_period == rv_carry_thru$cur_matchup_period)

      updateDateInput(
        session,
        "pin_date",
        value = cur_date,
        min = mup_min_max_dts$matchup_start,
        max = mup_min_max_dts$matchup_end
      )

      updateRadioButtons(session, "pin_dir", choices = c("-", "+"), selected = "+", inline = TRUE)
    }) |>
      bindEvent(rv_carry_thru$fty_parameters_met, rv_carry_thru$league_id, rv_carry_thru$competitor_id)

    observe({
      req(df_tbl())
      updateReactable("schedule-table", data = df_tbl(), selected = NA)
    }) |>
      bindEvent(rv_carry_thru$league_id, rv_carry_thru$competitor_id)

    # On matchup_selection change...
    observe({
      req(rv_carry_thru$fty_parameters_met)

      mup_min_max_dts <- pluck(dfs_fty_schedule, as.character(rv_carry_thru$league_id)) |>
        filter(
          matchup_period ==
            if (input$matchup_selection == "Post Fantasy") {
              99
            } else {
              as.numeric(str_extract(input$matchup_selection, "^\\d+ "))
            }
        ) |>
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
      bindEvent(input$matchup_selection, ignoreInit = TRUE, ignoreNULL = FALSE)

    # On copy_teams
    observe({
      req(rv_carry_thru$fty_parameters_met)

      selected_values <- dfs_fty_nba_mup_weeks |>
        pluck(as.character(rv_carry_thru$league_id), input$matchup_selection) |>
        select(Team) |>
        # mutate(Team = as.character(Team)) |>
        slice(getReactableState("schedule_table", "selected"))

      if (length(selected_values$Team) == 0) {
        show_toast(
          title = NULL,
          text = "No teams selected. Comparison not updated...",
          position = "bottom-start",
          type = "warning",
          timer = 2000
        )
      } else {
        rv_copy_teams(selected_values$Team)
        show_toast(
          title = NULL,
          text = "Teams added to comparison...",
          position = "bottom-start",
          type = "info",
          timer = 2000
        )
        updateReactable("schedule_table", selected = integer(0))
      }
    }) |>
      bindEvent(input$copy_teams, ignoreInit = TRUE)

    # Data Prep --------------------------------------------------------------

    # Pinned date calculations
    mup_dts <- reactive({
      req(rv_carry_thru$fty_parameters_met, input$matchup_selection != "")

      pluck(dfs_fty_schedule, as.character(rv_carry_thru$league_id)) |>
        filter(
          matchup_period ==
            if (input$matchup_selection == "Post Fantasy") {
              99
            } else {
              as.numeric(str_extract(input$matchup_selection, "^\\d+ "))
            }
        ) |>
        distinct(matchup_period, matchup_start, matchup_end)
    }) |>
      bindEvent(input$matchup_selection)

    # Pin index calc
    pin_ix <- reactive({
      req(nrow(mup_dts()) > 0)
      as.integer(difftime(input$pin_date, mup_dts()$matchup_start)) + 3
    }) |>
      bindEvent(input$pin_date)

    # Data for reactive table
    df_tbl <- reactive({
      req(pin_ix())

      max_range <- as.integer(difftime(mup_dts()$matchup_end, mup_dts()$matchup_start)) + 3

      dfs_fty_nba_mup_weeks |>
        pluck(as.character(rv_carry_thru$league_id), input$matchup_selection) |>
        rowwise() |>
        mutate(
          Pin = sum(c_across(
            if (
              (input$pin_date == mup_dts()$matchup_start & input$pin_dir == "-") |
                input$matchup_selection == "Post Fantasy"
            ) {
              0
            } else if (input$pin_dir == "+") {
              pin_ix():max_range
            } else {
              3:(pin_ix() - 1)
            }
          ))
        ) |>
        ungroup()
    }) |>
      bindEvent(input$matchup_selection, input$pin_dir, input$pin_date)

    # Schedule Table ---------------------------------------------------------

    # Table state
    rv_tbl_sort_order <- reactiveVal()
    cur_tbl_sort_order <- reactive(getReactableState("comparison_table", "sorted", session = session))
    observe(rv_tbl_sort_order(cur_tbl_sort_order())) |> bindEvent(cur_tbl_sort_order())

    output$schedule_table <- renderReactable({
      req(df_tbl())

      # Column formatting
      col_fmt <- map(set_names(str_subset(colnames(df_tbl()), "\\/")), \(x) {
        nm <- str_split_1(x, " ")
        colDef(
          header = tags$span(nm[1], tags$br(), nm[2]),
          filterInput = \(values, name) {
            tags$select(
              onchange = sprintf("Reactable.setFilter('schedule-table', '%s', event.target.value || undefined)", name),
              tags$option(value = "", ""),
              lapply(c(0, 1), tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px;"
            )
          },
          style = if (length(str_subset(colnames(df_tbl()), "\\/")) %% 7 > 0 & x %in% tail(colnames(df_tbl()), 2)) {
            list(backgroundColor = "#eee5ff94")
          } else if (parse_date_time(x, orders = "%a (%d/%m)") == input$pin_date) {
            list(backgroundColor = "#f1e78e94")
          }
        )
      })
      col_fmt[["Team"]] <- colDef(
        sticky = "left",
        style = list(backgroundColor = "#c1eccaff", fontWeight = "bold"),
        filterInput = \(values, name) {
          dataListId <- sprintf("%s-%s-list", 'schedule-table', name)
          tagList(
            tags$input(
              type = "text",
              list = dataListId,
              oninput = sprintf(
                "Reactable.setFilter('%s', '%s', event.target.value || undefined)",
                'schedule-table',
                name
              ),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px;"
            ),
            tags$datalist(
              id = dataListId,
              lapply(unique(values), function(value) tags$option(value = value))
            )
          )
        }
      )
      col_fmt[["Pin"]] <- colDef(
        sticky = "left",
        style = list(backgroundColor = "#c1eccaff", fontWeight = "bold", borderRight = "2px solid #0f0f0fff"),
        headerStyle = list(
          background = "#cce5ff",
          fontWeight = "bold",
          textAlign = "center",
          borderRight = "2px solid #0f0f0fff"
        ),
        filterMethod = JS(
          "function(rows, columnId, filterValue) {
            return rows.filter(function(row) {
              return row.values[columnId] >= filterValue
            })
          }"
        )
      )

      reactable(
        df_tbl(),
        defaultSorted = rv_tbl_sort_order(),
        defaultColDef = colDef(headerStyle = list(background = "#cce5ff", fontWeight = "bold", textAlign = "center")),
        selection = "multiple",
        filterable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        pagination = FALSE,
        height = "85vh",
        wrap = TRUE,
        columns = col_fmt,
        elementId = "schedule-table"
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
# library(lubridate)
# load("data/dfs_fty_nba_mup_weeks.rda")
# load("data/dfs_fty_schedule.rda")
# load("data/cur_date.rda")

# ui <- page_fluid(
#   mod_schedule_table_ui("schedule_table_1")
# )

# server <- function(input, output, session) {
# rv_carry_thru <- reactiveValues(
#   fty_parameters_met = TRUE,
#   platform = "ESPN",
#   league_id = 1382487116,
#   competitor_id = 6,
#   competitor_name = "britney_spears",
#   cur_matchup_period = 99
# )

#   mod_schedule_table_server("schedule_table_1", rv_carry_thru)
# }

# shinyApp(ui, server)
