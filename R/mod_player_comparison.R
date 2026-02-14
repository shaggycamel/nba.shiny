#' player_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput selectizeInput radioButtons sliderInput checkboxInput
#' @importFrom shinyWidgets switchInput
#' @importFrom bslib layout_sidebar sidebar card
#' @importFrom reactable reactableOutput
mod_player_comparison_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        switchInput(
          ns("team_player_switch"),
          value = TRUE,
          onLabel = "Team",
          offLabel = "Player",
          size = "small"
        ),
        selectInput(ns("team_player_names"), NULL, choices = ls_nba_teams, multiple = TRUE),
        selectizeInput(
          ns("excels_at_filter"),
          "Excels at",
          choices = character(0),
          options = list(maxItems = 5, onInitialize = I('function() { this.setValue(""); }'))
        ),
        radioButtons(ns("window"), "Rolling days", c(7, 15, 30), inline = TRUE),
        sliderInput(ns("minute_filter"), "Minute Filter", min = 0, max = 50, value = 20, round = TRUE),
        checkboxInput(ns("free_agent_filter"), "Free Agents only", value = TRUE),
      ),
      card(full_screen = TRUE, reactableOutput(ns("comparison_table"))),
      fillable = TRUE
    )
  )
}

#' player_comparison Server Functions
#'
#' @noRd
#' @importFrom dplyr select filter arrange desc mutate all_of
#' @importFrom purrr pluck map map_int discard discard_at set_names
#' @importFrom rlang is_null
#' @importFrom tidyr nest replace_na
#' @importFrom stringr str_detect str_like
#' @importFrom tibble deframe
#' @importFrom reactable reactable renderReactable colDef colFormat
mod_player_comparison_server <- function(id, carry_thru) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # On load...
    observe({
      req(carry_thru()$fty_parameters_met())

      updateSelectizeInput(
        session,
        "excels_at_filter",
        choices = pluck(
          ls_lo_lg_cats,
          as.character(carry_thru()$selected$league_id),
          "Categories"
        )
      )
    }) |>
      bindEvent(carry_thru()$fty_parameters_met()) # Bind event of when league is swapped too

    # On team player switch...
    observe({
      req(carry_thru()$fty_parameters_met())

      chs <- if (input$team_player_switch) {
        # When switch on Team
        ls_nba_teams
      } else {
        # When switch on Player
        ls <- dfs_player_comparison |>
          pluck(
            as.character(carry_thru()$selected$league_id),
            as.character(input$window)
          ) |>
          filter(!is.na(player_id), !is.na(player_name)) |>
          arrange(desc(min))

        if (input$free_agent_filter) {
          ls <- filter(ls, free_agent)
        }

        ls |>
          select(player_name, player_id) |>
          deframe() |>
          as.list()
      }

      updateSelectInput(session, "team_player_names", choices = chs)
    }) |>
      bindEvent(input$team_player_switch, input$free_agent_filter)

    df_comparison <- reactive({
      df <- dfs_player_comparison |>
        pluck(
          as.character(carry_thru()$selected$league_id),
          as.character(input$window)
        ) |>
        filter(min >= input$minute_filter)

      if (input$free_agent_filter) {
        df <- filter(df, free_agent)
      }

      if (!is_null(input$team_player_names)) {
        # FILTER TEAM OR PLAYER
        if (input$team_player_switch) {
          print(input$team_player_names)
          df <- filter(df, team_id %in% input$team_player_names)
        } else {
          print(input$team_player_names)
          df <- filter(df, player_id %in% input$team_player_names)
        }
      }

      if (!is_null(input$excels_at_filter)) {
        df <- filter(df, str_detect(`Excels At`, paste0(input$excels_at_filter, collapse = "|"))) |>
          mutate(xl_at_count = map_int(`Excels At`, \(x) sum(str_detect(x, input$excels_at_filter))))
      } else {
        df <- mutate(df, xl_at_count = 0)
      }

      df
    })

    output$comparison_table <- renderReactable({
      # Relevant cats
      cats <- ls_lo_lg_cats |>
        pluck(as.character(carry_thru()$select$league_id)) |>
        discard_at(\(x) x == "Overall") |>
        unlist(use.names = FALSE) |>
        discard(\(x) str_like(x, "%_pct"))

      # df creation
      df <- df_comparison() |>
        select(team, player = player_name, min, all_of(cats), `Excels At`, `Weak At`, inj_status, xl_at_count)

      # Column formatting
      col_fmt <- map(set_names(c("min", cats)), \(x) {
        colDef(style = function(value) {
          if (value == if (x == "tov") min(df[[x]], na.rm = TRUE) else max(df[[x]], na.rm = TRUE)) {
            list(background = "lightgreen", fontWeight = "bold")
          }
        })
      })
      col_fmt[["xl_at_count"]] <- colDef(show = FALSE)
      col_fmt[["Weak At"]] <- colDef(style = list(fontSize = "80%", whiteSpace = "pre-line"))
      col_fmt[["Excels At"]] <- colDef(style = \(val, ix) {
        xl_at <- df$xl_at_count[ix]
        if (xl_at == 1) {
          list(background = "#00CDCD", fontSize = "80%", whiteSpace = "pre-line")
        } else if (xl_at == 2) {
          list(background = "#8DEEEE", fontSize = "80%", whiteSpace = "pre-line")
        } else if (xl_at == 3) {
          list(background = "#00FFFF", fontSize = "80%", whiteSpace = "pre-line")
        } else {
          list(fontSize = "80%", whiteSpace = "pre-line")
        }
      })
      col_fmt[["inj_status"]] <- colDef(show = FALSE)
      col_fmt[["player"]] <- colDef(style = \(value, index) {
        status <- replace_na(df[["inj_status"]][index], "Available")
        if (status == "Out") {
          list(background = "#f51e1eff")
        } else if (status == "Questionable") {
          list(background = "#ffa87fff")
        } else if (status == "Probable") {
          list(background = "#fbff7fff")
        } else {
          list(background = "#ffffffff")
        }
      })

      reactable(
        df,
        pagination = FALSE,
        bordered = TRUE,
        highlight = TRUE,
        defaultSorted = list(min = "desc", player = "asc"),
        defaultSortOrder = "desc",
        defaultColDef = colDef(
          align = "left",
          minWidth = 120,
          headerStyle = list(background = "blue", color = "white"),
          format = colFormat(digits = 1)
        ),
        columns = col_fmt,
        onClick = "expand",
        details = function(ix) {
          # format this table nicely somehow...
          tm <- df$team[ix]
          if (is.na(tm)) {
            tags$div(tags$h2(class = "title", "Team not found..."))
          } else {
            tags$div(
              style = "margin-left: 10px; margin-top: 10px; margin-bottom: 30px;",
              tags$h2(
                class = "title",
                paste(tm, "Last", input$window, "Day Injury History: ")
              ),
              reactable(
                pluck(ls_injuries, input$window, tm, .default = tibble(!!tm := "No Injuries")),
                pagination = FALSE,
                fullWidth = FALSE,
                defaultColDef = colDef(
                  align = "left",
                  minWidth = 120,
                  headerStyle = list(background = "lightblue", color = "white")
                ),
                columns = list(player_names = colDef(minWidth = 1000))
              )
            )
          }
        }
      )
    })
  })
}
## To be copied in the UI
# mod_player_comparison_ui("player_comparison_1")

## To be copied in the server
# mod_player_comparison_server("player_comparison_1")

# library(shiny)
# library(bslib)
# library(reactable)
# library(stringr)
# library(rlang)
# library(purrr)
# library(dplyr)
# library(tibble)
# library(tidyr)
# library(shinyWidgets)

# load("data/dfs_player_comparison.rda")
# load("data/ls_lo_lg_cats.rda")
# load("data/ls_nba_teams.rda")
# load("data/ls_injuries.rda")

# ui <- page_fluid(
#   mod_player_comparison_ui("player_comparison_1")
# )

# server <- function(input, output, session) {
#   carry_thru <- reactiveVal(list(
#     fty_parameters_met = reactiveVal(TRUE),
#     selected = reactiveValues(
#       league_id = 24608
#     )
#   ))

#   mod_player_comparison_server("player_comparison_1", carry_thru)
# }

# shinyApp(ui, server)
