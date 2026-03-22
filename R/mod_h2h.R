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
        selectInput(ns("competitor"), NULL, choices = character(0)),
        selectInput(ns("matchup"), NULL, choices = 0),
        radioButtons(ns("window"), "Rolling days", c(7, 15, 30), inline = TRUE),
        actionButton(ns("alter_team"), "Alter Team"),
        # layout_columns(
        #   selectInput(ns("ex_player"), "Exclude", choices = character(0), multiple = TRUE),
        #   selectInput(ns("add_player"), "Add", choices = character(0), multiple = TRUE),
        # ),
        layout_columns(
          checkboxInput(ns("future_only"), "Future"),
          checkboxInput(ns("future_from_tomorrow"), "Tmrw")
        ),
        dateInput(ns("pin_date"), NULL),
        selectInput(ns("hl_player"), "Highlight Player", choices = character(0), multiple = TRUE),
        # selectInput(ns("log_config"), "Log Filter Config", choices = character(0), size = 4, selectize = FALSE),
        # actionButton(ns("snapshot_config"), "Snapshot config"),
      ),
      card(
        height = 1400,
        fill = FALSE,
        card(full_screen = TRUE, min_height = 500, max_height = 700, plotlyOutput(ns("stat_plot"))),
        card(
          full_screen = TRUE,
          min_height = 200,
          max_height = 650,
          # tagList(
          #   tags$div(
          #     style = "overflow-x: auto; white-space: nowrap; padding: 5px;",
          #     tags$div(style = "width: 1200px;", reactableOutput(ns("game_table_sum"))),
          #     br(),
          #     tags$div(style = "width: 1200px;", reactableOutput(ns("game_table_player")))
          #   )
          # )
          reactableOutput(ns("game_table"))
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
        choices = dfs_h2h_future |>
          pluck(
            as.character(carry_thru()$selected$league_id),
            "free_agent",
            "free_agent",
            input$window
          ) |>
          arrange(desc(min)) |>
          distinct(player_name, player_id) |>
          na.omit() |>
          deframe()
      )

      # updateSelectInput(session, "log_config", choices = ls_log_config)
    }) |>
      bindEvent(carry_thru()$fty_parameters_met())

    # Initial date picker, relies on df_base
    observe({
      req(df_base())

      updateDateInput(
        session,
        "pin_date",
        value = cur_date,
        min = unique(na.omit(df_base()$matchup_start)),
        max = unique(na.omit(df_base()$matchup_end))
      )
    }) |>
      bindEvent(df_base(), once = TRUE)

    # Ongoing Date picker pin_date
    observe({
      req(df_base())
      updateDateInput(
        session,
        "pin_date",
        value = if (cur_date > unique(na.omit(df_base()$matchup_end))) {
          unique(na.omit(df_base()$matchup_end))
        } else {
          cur_date
        },
        min = if (cur_date > unique(na.omit(df_base()$matchup_end))) {
          unique(na.omit(df_base()$matchup_end))
        } else {
          unique(na.omit(df_base()$matchup_start))
        },
        max = unique(na.omit(df_base()$matchup_end))
      )
    }) |>
      bindEvent(input$matchup)

    # Eventually replace with modal
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

    # observe({
    #   lst(
    #     "competitor" = input$competitor,
    #     "matchup" = input$matchup,
    #     "window" = input$window,
    #     "ex_player" = input$ex_player,
    #     "add_player" = input$add_player,
    #     "future_only" = input$future_only,
    #     "future_from_tomorrow" = input$future_from_tomorrow,
    #     "hl_player" = input$hl_player
    #   )
    # }) |>
    #   bindEvent(input$snapshot_config)

    # Data prep --------------------------------------------------------------

    # Create generic function to calc this
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
                  "Total - ", round(sum(!!m_col, na.rm = TRUE), 0), "/", round(sum(!!a_col, na.rm = TRUE), 0), " (", label_percent(accuracy = 0.01)(sum(!!m_col, na.rm = TRUE)/sum(!!a_col, na.rm = TRUE)),")\n\n",
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
      req(df_base(), df_grey_player())

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
        ) |>
        select(-starts_with("NA")) |>
        rowwise() |>
        mutate(
          games_remaining = if (cur_date > unique(na.omit(df_base()$matchup_end))) {
            0
          } else {
            sum(as.numeric(str_remove(c_across((pin_ix() + 4):last_col()), "\\*")), na.rm = TRUE)
          },
          .before = if (all(is.na(df_base()$matchup_end_plus))) last_col() else last_col(2)
        ) |>
        ungroup() |>
        left_join(df_grey_player(), by = join_by(player_id))
    })

    df_tbl_sum <- reactive({
      req(df_tbl())

      df_tbl() |>
        summarise(across(contains("/"), \(x) sum(as.numeric(x), na.rm = TRUE)), .by = competitor) |>
        arrange(desc(competitor)) |>
        rename(player_team = competitor) |>
        mutate(player_name = NA, .after = player_team) |>
        rowwise() |>
        mutate(
          games_remaining = if (cur_date > unique(na.omit(df_base()$matchup_end))) {
            0
          } else {
            sum(as.numeric(str_remove(c_across((pin_ix() + 2):last_col()), "\\*")), na.rm = TRUE)
          },
          .before = if (all(is.na(df_base()$matchup_end_plus))) last_col() else last_col(2)
        ) |>
        ungroup() |>
        mutate(min_grey_date = NA_Date_, max_grey_date = NA_Date_)
    })

    pin_ix <- reactive({
      req(df_base())
      df_base() |>
        distinct(game_date) |>
        na.omit() |>
        pull(game_date) |>
        sort() |>
        detect_index(\(x) x == input$pin_date)
    }) |>
      bindEvent(input$pin_date)

    # Potentially turn this into a static df - haven't thought it thru yet
    # Haven't tested on whehter players can be added/excluded and still greyed out...
    df_grey_player <- reactive({
      req(df_base())

      # past roster
      df_base() |>
        filter(tense == "past") |>
        summarise(
          min_grey_date = min(game_date),
          max_grey_date = max(game_date),
          .by = player_id
        ) |>
        inner_join(
          # Current Roster
          pluck(dfs_fty_roster, as.character(carry_thru()$selected$league_id)) |>
            filter(
              competitor_id %in% c(carry_thru()$selected$competitor_id, opponent_id()),
              matchup_period == as.integer(input$matchup)
            ) |>
            mutate(max_assigned_date = max(assigned_date)) |>
            filter(
              min(assigned_date) != matchup_start |
                max(assigned_date) != max_assigned_date,
              .by = player_id
            ) |>
            distinct(player_id, matchup_start, max_assigned_date),
          by = join_by(player_id)
        ) |>
        mutate(
          min_grey_date = if_else(
            min_grey_date == matchup_start,
            NA_Date_,
            min_grey_date
          ),
          max_grey_date = if_else(
            between(max_grey_date, max_assigned_date - 1, max_assigned_date),
            NA_Date_,
            max_grey_date
          )
        ) |>
        select(-c(matchup_start, max_assigned_date))
    })

    # Plot -------------------------------------------------------------------

    output$stat_plot <- renderPlotly({
      req(df_plt())

      ggplotly(
        df_plt() |>
          ggplot(aes(x = name, y = value, fill = competitor, text = label)) +
          geom_col(position = "fill") +
          geom_hline(aes(yintercept = 0.5)) +
          # scale_y_continuous(labels = scales::label_percent()) +
          scale_fill_brewer(type = "qual", palette = "Set2", direction = -1) +
          theme_bw() +
          labs(x = NULL, y = NULL),
        tooltip = "text"
      ) |>
        layout(hovermode = "x") |>
        config(displayModeBar = FALSE)
    })

    # Game Table -------------------------------------------------------------

    output$game_table <- renderReactable({
      req(df_tbl_sum(), df_tbl(), df_grey_player())

      col_fmt <- game_tbl_col_fmt(df_tbl(), input$pin_date, unique(na.omit(df_base()$matchup_end)))
      col_fmt_sum <- game_tbl_col_fmt(df_tbl_sum(), input$pin_date, unique(na.omit(df_base()$matchup_end)), "sum")

      reactable(
        df_tbl_sum(),
        pagination = FALSE,
        bordered = TRUE,
        style = list(border = "1px solid #000000"),
        rowStyle = list(borderBottom = "1px solid #000000"),
        theme = reactableTheme(headerStyle = list(borderBottom = "1px solid #000000")),
        highlight = TRUE,
        sortable = FALSE,
        defaultColDef = colDef(headerStyle = list(background = "#cce5ff")),
        columns = col_fmt_sum,
        defaultExpanded = TRUE,
        details = \(ix) {
          tags$div(
            style = "margin-left: 45px; margin-top: 10px; margin-bottom: 30px;",
            reactable(
              filter(df_tbl(), competitor == df_tbl_sum()$player_team[ix]),
              pagination = FALSE,
              bordered = TRUE,
              style = list(border = "1px solid #000000"),
              highlight = TRUE,
              theme = reactableTheme(headerStyle = list(display = "none")),
              defaultSorted = list(player_team = "asc", player_name = "asc"),
              defaultColDef = colDef(headerStyle = list(background = "#cce5ff")), # Not sure if needed
              columns = col_fmt,
              rowStyle = function(index) {
                if (df_tbl()$player_id[index] %in% as.numeric(input$hl_player)) {
                  list(backgroundColor = "#ffef9dff", fontWeight = "bold") # Light yellow highlight
                }
              },
            )
          )
        }
      )
    })

    # output$game_table_sum <- renderReactable({
    #   req(df_tbl_sum())

    #   col_fmt <- game_tbl_col_fmt(df_tbl_sum(), input$pin_date, unique(na.omit(df_base()$matchup_end)), "sum")

    #   reactable(
    #     df_tbl_sum(),
    #     pagination = FALSE,
    #     bordered = TRUE,
    #     style = list(border = "1px solid #000000"),
    #     highlight = TRUE,
    #     sortable = FALSE,
    #     defaultColDef = colDef(headerStyle = list(background = "#cce5ff")),
    #     columns = col_fmt,
    #   )
    # })

    # output$game_table_player <- renderReactable({
    #   req(df_tbl())

    #   df <- filter(
    #     df_tbl(),
    #     competitor ==
    #       pluck(ls_fty_lookup, "cp_id_to_name", as.character(carry_thru()$selected$league_id), input$competitor)
    #   ) |>
    #     left_join(df_grey_player(), by = join_by(player_id))

    #   col_fmt <- game_tbl_col_fmt(df, input$pin_date, unique(na.omit(df_base()$matchup_end)))

    #   reactable(
    #     df,
    #     pagination = FALSE,
    #     bordered = TRUE,
    #     style = list(border = "1px solid #000000"),
    #     highlight = TRUE,
    #     theme = reactableTheme(headerStyle = list(display = "none")),
    #     defaultSorted = list(player_team = "asc", player_name = "asc"),
    #     defaultColDef = colDef(headerStyle = list(background = "#cce5ff")),
    #     columns = col_fmt,
    #     rowStyle = function(index) {
    #       if (df$player_id[index] %in% as.numeric(input$hl_player)) {
    #         list(backgroundColor = "#ffef9dff", fontWeight = "bold") # Light yellow highlight
    #       }
    #     }
    #   )
    # })
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
library(lubridate)

load("data/cur_date.rda")
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
      league_id = 1382487116,
      competitor_id = 6,
      cur_matchup_period = 21
    )
  ))

  mod_h2h_server("h2h_1", carry_thru)
}

shinyApp(ui, server)
