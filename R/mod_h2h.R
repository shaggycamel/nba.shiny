#' h2h UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_h2h_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        layout_columns(
          selectInput(ns("competitor"), "Competitor", choices = character(0)),
          selectInput(ns("matchup"), "Matchup", choices = 0)
        ),
        radioButtons(ns("window"), "Rolling days", c(7, 15, 30), inline = TRUE),
        layout_columns(
          selectInput(ns("ex_player"), "Exclude", choices = character(0), multiple = TRUE),
          selectInput(ns("add_player"), "Add", choices = character(0), multiple = TRUE),
        ),
        layout_columns(
          checkboxInput(ns("future_only"), "Future"),
          checkboxInput(ns("future_from_tomorrow"), "Tmrw")
        ),
        selectInput(ns("hl_player"), "Highlight Player", choices = character(0), multiple = TRUE),
        selectInput(ns("log_config"), "Log Filter Config", choices = character(0), size = 4, selectize = FALSE),
        actionButton(ns("snapshot_config"), "Snapshot config"),
      ),
      card(
        height = 1400,
        fill = FALSE,
        card(full_screen = TRUE, min_height = 500, max_height = 700, plotlyOutput(ns("stat_plot"))),
        card(
          full_screen = TRUE,
          min_height = 200,
          max_height = 650,
          tagList(
            tags$div(
              style = "overflow-x: auto; white-space: nowrap; padding: 5px;",
              tags$div(style = "width: 1200px;", reactableOutput(ns("game_table_sum"))),
              br(),
              tags$div(style = "width: 1200px;", reactableOutput(ns("game_table_player")))
            )
          )
        )
      ),
      fillable = TRUE,
      tags$style(
        type = "text/css",
        ".selectize-dropdown-content{width: 200px;background-color: #FFFFFF; align: right;}"
      )
    )
  )
}


#' h2h Server Functions
#'
#' @noRd
#'
mod_h2h_server <- function(id, carry_thru) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update UI --------------------------------------------------------------

    observe({
      req(carry_thru()$fty_parameters_met())

      updateSelectInput(
        session,
        "competitor",
        choices = pluck(ls_fty_lookup, "cp_name_to_id", as.character(carry_thru()$selected$league_id)),
        selected = carry_thru()$selected$competitor_id
      )
      updateSelectInput(
        session,
        "matchup",
        choices = sort(unique((pluck(dfs_fty_schedule, as.character(carry_thru()$selected$league_id)))$matchup_period)),
        selected = carry_thru()$selected$cur_matchup_period
      )
      updateSelectInput(
        session,
        "hl_player",
        choices = pluck(dfs_fty_roster, as.character(carry_thru()$selected$league_id)) |>
          filter(competitor_id == carry_thru()$selected$competitor_id) |>
          slice_max(assigned_date) |>
          arrange(player_name) |>
          select(player_name, player_id) |>
          na.omit() |>
          deframe()
      )
      updateSelectInput(
        session,
        "ex_player",
        choices = pluck(dfs_fty_roster, as.character(carry_thru()$selected$league_id)) |>
          filter(competitor_id == carry_thru()$selected$competitor_id) |>
          slice_max(assigned_date) |>
          arrange(player_name) |>
          select(player_name, player_id) |>
          na.omit() |>
          deframe()
      )
      updateSelectInput(
        session,
        "add_player",
        choices = dfs_h2h_today |>
          pluck(
            as.character(carry_thru()$selected$league_id),
            "free_agent",
            "free_agent",
            input$window
          ) |>
          arrange(desc(!!sym("min"))) |>
          select(player_name, player_id) |>
          na.omit() |>
          deframe()
      )
      # updateSelectInput(session, "log_config", choices = ls_log_config)
    }) |>
      bindEvent(carry_thru()$fty_parameters_met())

    observe({
      req(carry_thru()$fty_parameters_met())

      updateSelectInput(
        session,
        "ex_player",
        choices = dfs_fty_roster |>
          pluck(as.character(carry_thru()$selected$league_id)) |>
          filter(competitor_id == input$competitor) |>
          slice_max(assigned_date) |>
          arrange(player_name) |>
          select(player_name, player_id) |>
          na.omit() |>
          deframe()
      )
    }) |>
      bindEvent(input$competitor)

    observe({
      req(df_base())
      players_already_hl <- setdiff(input$hl_player, input$ex_player)

      updateSelectInput(
        session,
        "hl_player",
        choices = df_base() |>
          filter(
            competitor ==
              pluck(ls_fty_lookup, "cp_id_to_name", as.character(carry_thru()$selected$league_id), input$competitor)
          ) |>
          arrange(player_name) |>
          select(player_name, player_id) |>
          na.omit() |>
          deframe(),
        selected = players_already_hl
      )
    }) |>
      bindEvent(input$add_player, input$ex_player)

    observe({
      lst(
        "competitor" = input$competitor,
        "matchup" = input$matchup,
        "window" = input$window,
        "ex_player" = input$ex_player,
        "add_player" = input$add_player,
        "future_only" = input$future_only,
        "future_from_tomorrow" = input$future_from_tomorrow,
        "hl_player" = input$hl_player
      )
    }) |>
      bindEvent(input$snapshot_config)

    # Data prep --------------------------------------------------------------

    opponent_id <- reactiveVal()
    observe({
      req(carry_thru()$fty_parameters_met())
      opponent_id(
        pluck(dfs_fty_schedule, as.character(carry_thru()$selected$league_id)) |>
          filter(
            competitor_id == as.numeric(input$competitor),
            matchup_period == as.numeric(input$matchup)
          ) |>
          pull(opponent_id)
      )
    }) |>
      bindEvent(input$competitor, input$matchup)

    df_base <- reactive({
      req(opponent_id())

      l_id <- as.character(carry_thru()$selected$league_id)
      map(c(input$competitor, opponent_id()), \(x) {
        bind_rows(
          compact(
            lst(
              "past" = if (input$future_only) {
                NULL
              } else {
                pluck(dfs_h2h_past, l_id, input$matchup, x, .default = tibble(player_id = 0))
              },

              "today" = if (input$future_only & input$future_from_tomorrow) {
                NULL
              } else {
                if (
                  x != input$competitor |
                    input$future_from_tomorrow |
                    as.integer(input$matchup) < carry_thru()$selected$cur_matchup_period
                ) {
                  pluck(dfs_h2h_today, l_id, input$matchup, x, input$window, .default = tibble(player_id = 0))
                } else {
                  pluck(dfs_h2h_today, l_id, input$matchup, x, input$window, .default = tibble(player_id = 0)) |>
                    filter(!player_id %in% (as.integer(input$ex_player) %||% 0)) |>
                    bind_rows(
                      dfs_h2h_today |>
                        pluck(l_id, "free_agent", "free_agent", input$window, .default = tibble(player_id = 0)) |>
                        filter(player_id %in% (as.integer(input$add_player) %||% 0))
                    )
                }
              },

              "future" = if (
                x != input$competitor |
                  as.integer(input$matchup) < carry_thru()$selected$cur_matchup_period
              ) {
                pluck(dfs_h2h_future, l_id, input$matchup, x, input$window, .default = tibble(player_id = 0))
              } else {
                dfs_h2h_future |>
                  pluck(l_id, input$matchup, x, input$window, .default = tibble(player_id = 0)) |>
                  filter(!player_id %in% (as.integer(input$ex_player) %||% 0)) |>
                  bind_rows(
                    dfs_h2h_future |>
                      pluck(l_id, "free_agent", "free_agent", input$window, .default = tibble(player_id = 0)) |>
                      filter(player_id %in% (as.integer(input$add_player) %||% 0))
                  )
              }
            )
          ),
          .id = "tense"
        ) |>
          mutate(competitor = pluck(ls_fty_lookup, "cp_id_to_name", l_id, x))
      }) |>
        list_rbind() |>
        mutate(
          competitor = ordered(
            competitor,
            c(
              pluck(ls_fty_lookup, "cp_id_to_name", l_id, as.character(opponent_id())),
              pluck(ls_fty_lookup, "cp_id_to_name", l_id, input$competitor)
            )
          )
        )
    })

    df_plt <- reactive({
      req(df_base())

      df_base() |>
        select(
          competitor,
          player_id,
          player_name,
          inj_status,
          matches("f[g|t][m|a]"),
          any_of(unname(pluck(ls_lo_lg_cats, as.character(carry_thru()$selected$league_id))[["Categories"]]))
        ) |>
        pivot_longer(c(
          matches("f[g|t][m|a]"),
          any_of(unname(pluck(ls_lo_lg_cats, as.character(carry_thru()$selected$league_id))[["Categories"]]))
        )) |>
        mutate(value = if_else(inj_status == "Out", 0, value, missing = value)) |>
        summarise(
          value = sum(value, na.rm = TRUE),
          .by = c(competitor, player_id, player_name, name)
        ) |>
        arrange(desc(value)) |>
        (\(x) {
          f_inr <- \(col_string) {
            m_col <- sym(str_c(col_string, "m"))
            a_col <- sym(str_c(col_string, "a"))

            x |>
              filter(str_detect(name, col_string)) |>
              pivot_wider(names_from = name, values_from = value) |>
              summarise(
                #fmt: skip
                label = paste0(
                  "Total - ", round(sum(!!m_col, na.rm = TRUE), 0), "/", round(sum(!!a_col, na.rm = TRUE), 0), " (", percent(sum(!!m_col, na.rm = TRUE)/sum(!!a_col, na.rm = TRUE), accuracy = 0.01),")\n\n",
                  paste(str_c(player_name, " - ", round(!!m_col, 0), "/", round(!!a_col, 0)), collapse = "\n")
                ),
                name = str_c(col_string, "_pct"),
                value = sum(!!m_col, na.rm = TRUE) / sum(!!a_col, na.rm = TRUE),
                .by = competitor
              )
          }

          bind_rows(
            f_inr("fg"),
            f_inr("ft"),
            x |>
              filter(!str_like(name, "f[g|t][m|a]")) |>
              summarise(
                # fmt: skip
                label = paste0(
                  "Total - ", round(sum(value, na.rm = TRUE), 1), "\n\n",
                  paste(str_c(player_name, " - ", round(value, 1)), collapse = "\n")
                ),
                value = sum(value, na.rm = TRUE),
                .by = c(competitor, name)
              )
          )
        })()
    })

    df_tbl <- reactive({
      req(df_base())

      df_base() |>
        arrange(game_date) |>
        select(competitor, player_team, player_id, player_name, inj_status, fmt_date, scheduled_to_play) |>
        distinct() |>
        mutate(
          scheduled_to_play = as.character(replace_na(scheduled_to_play, 0)),
          scheduled_to_play = if_else(
            inj_status == "Out",
            str_c(scheduled_to_play, "*"),
            scheduled_to_play,
            missing = scheduled_to_play
          )
        ) |>
        select(-inj_status) |>
        pivot_wider(
          names_from = fmt_date,
          values_from = scheduled_to_play,
          values_fill = "0"
        )
    })

    df_tbl_sum <- reactive({
      req(df_tbl())

      df_tbl() |>
        summarise(across(contains("/"), \(x) sum(as.numeric(x), na.rm = TRUE)), .by = competitor) |>
        arrange(desc(competitor)) |>
        rename(player_team = competitor) |>
        mutate(player_name = NA, .after = player_team)
    })

    # Plot -------------------------------------------------------------------

    output$stat_plot <- renderPlotly({
      req(df_plt())

      df <- df_plt()

      ggplotly(
        df |>
          ggplot(aes(x = name, y = value, fill = competitor, text = label)) +
          geom_col(position = "fill") +
          geom_hline(aes(yintercept = 0.5)) +
          # scale_y_continuous(labels = scales::label_percent()) +
          scale_fill_brewer(type = "qual", palette = "Set2", direction = -1) +
          theme_bw() +
          labs(
            x = NULL,
            y = NULL
          ),
        tooltip = "text"
      ) |>
        layout(hovermode = "x") |>
        config(displayModeBar = FALSE)
    })

    # Game Table -------------------------------------------------------------

    output$game_table_sum <- renderReactable({
      req(df_tbl_sum())

      df <- df_tbl_sum()
      col_fmt <- game_tbl_col_fmt(df, "sum")

      reactable(
        df,
        pagination = FALSE,
        bordered = TRUE,
        style = list(border = "1px solid #000000"),
        highlight = TRUE,
        sortable = FALSE,
        defaultColDef = colDef(headerStyle = list(background = "#cce5ff")),
        columns = col_fmt,
      )
    })

    output$game_table_player <- renderReactable({
      req(df_tbl())

      df <- filter(
        df_tbl(),
        competitor ==
          pluck(ls_fty_lookup, "cp_id_to_name", as.character(carry_thru()$selected$league_id), input$competitor)
      )
      col_fmt <- game_tbl_col_fmt(df)

      reactable(
        df,
        pagination = FALSE,
        bordered = TRUE,
        style = list(border = "1px solid #000000"),
        highlight = TRUE,
        theme = reactableTheme(headerStyle = list(display = "none")),
        defaultSorted = list(player_team = "asc", player_name = "asc"),
        defaultColDef = colDef(headerStyle = list(background = "#cce5ff")),
        columns = col_fmt,
        rowStyle = function(index) {
          if (df$player_id[index] %in% as.numeric(input$hl_player)) {
            list(backgroundColor = "#ffef9dff", fontWeight = "bold") # Light yellow highlight
          }
        }
      )
    })
  })
}

## To be copied in the UI
# mod_h2h_ui("h2h_1")

## To be copied in the server
# mod_h2h_server("h2h_1")

library(shiny)
library(bslib)
library(shinyWidgets)
library(reactable)
library(plotly)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(tidyr)
library(scales)
load("data/ls_fty_lookup.rda")
load("data/ls_lo_lg_cats.rda")
load("data/dfs_fty_schedule.rda")
load("data/dfs_fty_roster.rda")
load("data/dfs_h2h_past.rda")
load("data/dfs_h2h_today.rda")
load("data/dfs_h2h_future.rda")
source("R/fct_game_tbl_col_fmt.R")

ui <- page_fluid(
  mod_h2h_ui("h2h_1")
)

server <- function(input, output, session) {
  carry_thru <- reactiveVal(list(
    fty_parameters_met = reactiveVal(TRUE),
    selected = reactiveValues(
      platform = "ESPN",
      league_id = 1966813226,
      competitor_id = 5, # 25
      competitor_name = "britney_spears",
      cur_matchup_period = 19
    )
  ))

  mod_h2h_server("h2h_1", carry_thru)
}

shinyApp(ui, server)
