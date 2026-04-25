#' league_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_league_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(".selectize-dropdown-content{min-width: 100%; box-sizing: border-box;}"))
    ),
    layout_sidebar(
      sidebar = sidebar(
        selectInput(ns("fty_lg_ov_cat"), "Category", choices = character(0)),
        switchInput(ns("fty_lg_ov_rank_toggle"), value = TRUE, onLabel = "Rank", offLabel = "Value", size = "small"),
        switchInput(ns("fty_lg_ov_cum_toggle"), value = TRUE, onLabel = "W2W", offLabel = "Cum", size = "small"),
        checkboxInput(ns("fty_lg_ov_just_h2h"), "Just H2H")
      ),
      card(
        height = 1250,
        fill = FALSE,
        card(full_screen = TRUE, min_height = 500, max_height = 600, plotlyOutput(ns("fty_lo_plt"))),
        card(full_screen = TRUE, min_height = 200, max_height = 650, reactableOutput(ns("tbl_recent_activity")))
      )
    )
  )
}


#' league_overview Server Functions
#'
#' @noRd
#'
mod_league_overview_server <- function(id, rv_carry_thru) {
  moduleServer(id, function(input, output, session) {
    # Update categories ------------------------------------------------------

    observe({
      req(rv_carry_thru$fty_parameters_met)
      updateSelectInput(
        session = session,
        inputId = "fty_lg_ov_cat",
        choices = pluck(ls_lo_lg_cats, as.character(rv_carry_thru$league_id))
      )
    }) |>
      bindEvent(rv_carry_thru$fty_parameters_met) # Bind event of when league is swapped too

    # Data prep --------------------------------------------------------------

    df_lo <- reactive(pluck(dfs_league_overview, as.character(rv_carry_thru$league_id)))
    df_lo_pt <- reactive(filter(df_lo(), as.integer(matchup_sigmoid) == matchup_sigmoid))
    opponent <- reactive(get_opponent(rv_carry_thru, rv_carry_thru$cur_matchup_period))

    df_tbl <- reactive({
      req(rv_carry_thru$fty_parameters_met)

      if (input$fty_lg_ov_just_h2h) {
        pluck(dfs_fty_recent_activity, as.character(rv_carry_thru$league_id)) |>
          filter(competitor_id %in% c(rv_carry_thru$competitor_id, opponent()$id))
      } else {
        pluck(dfs_fty_recent_activity, as.character(rv_carry_thru$league_id))
      }
    }) |>
      bindEvent(rv_carry_thru$fty_parameters_met, input$fty_lg_ov_just_h2h)

    # Plot -------------------------------------------------------------------

    output$fty_lo_plt <- renderPlotly({
      req(rv_carry_thru$fty_parameters_met, df_lo())

      plot_col <- input$fty_lg_ov_cat
      if (input$fty_lg_ov_rank_toggle) {
        plot_col <- str_c(plot_col, "_rank")
      }

      req(plot_col %in% colnames(df_lo()))
      plt <- if (input$fty_lg_ov_cum_toggle) {
        df_lo() |>
          ggplot(aes(x = matchup_sigmoid, y = !!sym(plot_col), colour = competitor_name)) +
          geom_line(linewidth = 0.5) +
          geom_point(aes(text = !!sym(paste0(str_remove(plot_col, "_rank"), "_text"))), data = df_lo_pt(), size = 2) +
          scale_x_continuous(breaks = sort(unique(df_lo_pt()$matchup)), labels = sort(unique(df_lo_pt()$matchup))) +
          labs(
            title = paste("Competitor Category Ranking:", input$fty_lg_ov_cat),
            x = "Matchup Period",
            y = input$fty_lg_ov_cat
          ) +
          theme_bw()
      } else {
        df_lo_pt() |>
          arrange(matchup) |>
          mutate(across(-matches("_id$|^matchup"), \(x) cumsum(x)), .by = competitor_name) |>
          ggplot(aes(x = matchup_sigmoid, y = !!sym(plot_col), colour = competitor_name)) +
          geom_path() +
          scale_x_continuous(
            breaks = sort(unique(df_lo_pt()$matchup)),
            labels = sort(unique(df_lo_pt()$matchup))
          ) +
          labs(
            title = paste("Competitor Category Ranking:", input$fty_lg_ov_cat),
            x = "Matchup Period",
            y = input$fty_lg_ov_cat
          ) +
          theme_bw()
      }

      if (input$fty_lg_ov_rank_toggle) {
        plt <- plt + scale_y_reverse(n.breaks = length(unique(df_lo()$competitor_id)))
      }

      n_competitors <- length(unique(df_lo()$competitor_id))
      plt <- ggplotly(plt, tooltip = "text") |>
        # Suppress tooltips on line traces (first n traces), keep points
        style(hoverinfo = "none", traces = seq_len(n_competitors)) |>
        # Apply hovertemplate to point traces so HTML renders
        style(
          hovertemplate = ~ paste0(fg3_m_text, "<extra></extra>"),
          traces = seq_len(n_competitors) + n_competitors
        ) |>
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) |>
        rangeslider(
          start = ifelse(!input$fty_lg_ov_cum_toggle, 1, max(df_lo_pt()$matchup) - 5.1),
          end = max(df_lo_pt()$matchup) + 0.1,
          range = list(min(df_lo_pt()$matchup) - 0.2, max(df_lo_pt()$matchup) + 0.2)
        ) |>
        config(displayModeBar = FALSE)

      if (input$fty_lg_ov_just_h2h) {
        just_h2h <- setdiff(
          1:length(plt$x$data),
          str_which(
            map_chr(plt$x$data, \(x) x$name),
            paste0(rv_carry_thru$competitor_name, "|", opponent()$name)
          )
        )

        plt <- style(plt, visible = "legendonly", traces = just_h2h)
      }
      plt
    })

    # Table ------------------------------------------------------------------

    output$tbl_recent_activity <- renderReactable({
      req(df_tbl())

      col_fmt <- list(
        player = colDef(name = "Player"),
        competitor_id = colDef(show = FALSE),
        competitor_name = colDef(
          name = "Competitor",
          filterInput = \(values, name) {
            tags$select(
              onchange = sprintf(
                "Reactable.setFilter('league-overview-table', '%s', event.target.value || undefined)",
                name
              ),
              tags$option(value = "", ""),
              lapply(unique(values), tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px;"
            )
          }
        ),
        action = colDef(
          name = "Action",
          filterInput = \(values, name) {
            tags$select(
              onchange = sprintf(
                "Reactable.setFilter('league-overview-table', '%s', event.target.value || undefined)",
                name
              ),
              tags$option(value = "", ""),
              lapply(unique(values), tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px;"
            )
          }
        ),
        timestamp = colDef(name = "Time (EST)", cell = \(value) {
          format(value, "%a %d/%m %H:%M", tz = "America/New_York")
        })
      )

      reactable(
        df_tbl(),
        pagination = FALSE,
        bordered = TRUE,
        style = list(border = "1px solid #000000"),
        highlight = TRUE,
        filterable = TRUE,
        defaultSorted = list(timestamp = "desc"),
        defaultColDef = colDef(headerStyle = list(background = "#cce5ff")),
        columns = col_fmt,
        elementId = "league-overview-table"
      )
    })
  })
}

## To be copied in the UI
# mod_league_overview_ui("league_overview_1")

## To be copied in the server
# mod_league_overview_server("league_overview_1")

# library(shiny)
# library(bslib)
# library(shinyWidgets)
# library(plotly)
# library(stringr)
# library(purrr)
# library(dplyr)
# library(tidyr)
# library(reactable)

# load("data/dfs_league_overview.rda")
# load("data/dfs_fty_recent_activity.rda")
# load("data/ls_lo_lg_cats.rda")

# source("R/utils_get_opponent.R")

# ui <- page_fluid(
#   mod_league_overview_ui("league_overview_1")
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

#   mod_league_overview_server("league_overview_1", rv_carry_thru)
# }

# shinyApp(ui, server)
