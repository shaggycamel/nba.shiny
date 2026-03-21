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
mod_league_overview_server <- function(id, carry_thru) {
  moduleServer(id, function(input, output, session) {
    # Update categories ------------------------------------------------------

    observe({
      req(carry_thru()$fty_parameters_met())
      updateSelectInput(
        session = session,
        inputId = "fty_lg_ov_cat",
        choices = pluck(ls_lo_lg_cats, as.character(carry_thru()$selected$league_id))
      )
    }) |>
      bindEvent(carry_thru()$fty_parameters_met()) # Bind event of when league is swapped too

    # Data prep --------------------------------------------------------------

    df_lo <- reactive(pluck(dfs_league_overview, as.character(carry_thru()$selected$league_id)))
    df_lo_pt <- reactive(filter(df_lo(), as.integer(matchup_sigmoid) == matchup_sigmoid))

    df_tbl <- reactive({
      req(carry_thru()$fty_parameters_met())

      if (input$fty_lg_ov_just_h2h) {
        pluck(dfs_fty_recent_activity, as.character(carry_thru()$selected$league_id)) |>
          filter(competitor_id %in% c(carry_thru()$selected$competitor_id, carry_thru()$selected$opponent_id))
      } else {
        pluck(dfs_fty_recent_activity, as.character(carry_thru()$selected$league_id))
      }
    }) |>
      bindEvent(carry_thru()$fty_parameters_met(), input$fty_lg_ov_just_h2h)

    # Plot -------------------------------------------------------------------

    output$fty_lo_plt <- renderPlotly({
      req(carry_thru()$fty_parameters_met(), df_lo())

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
            paste0(carry_thru()$selected$competitor_name, "|", carry_thru()$selected$opponent_name)
          )
        )

        plt <- style(plt, visible = "legendonly", traces = just_h2h)
      }
      plt
    })

    # Table ------------------------------------------------------------------

    output$tbl_recent_activity <- renderReactable({
      req(df_tbl())

      reactable(
        df_tbl(),
        pagination = FALSE,
        bordered = TRUE,
        style = list(border = "1px solid #000000"),
        highlight = TRUE,
        filterable = TRUE,
        defaultSorted = list(timestamp = "desc"),
        defaultColDef = colDef(headerStyle = list(background = "#cce5ff")),
        columns = list(
          competitor_id = colDef(show = FALSE),
          competitor_name = colDef(name = "Competitor"),
          player = colDef(name = "Player"),
          action = colDef(name = "Action"),
          timestamp = colDef(name = "Time (EST)", cell = \(value) {
            format(value, "%a %d/%m %H:%M", tz = "America/New_York")
          })
        )
      )
    })
  })
}

## To be copied in the UI
# mod_league_overview_ui("league_overview_1")

## To be copied in the server
# mod_league_overview_server("league_overview_1")

library(shiny)
library(bslib)
library(shinyWidgets)
library(plotly)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)

load("data/dfs_league_overview.rda")
load("data/dfs_fty_recent_activity.rda")
load("data/ls_lo_lg_cats.rda")


ui <- page_fluid(
  mod_league_overview_ui("league_overview_1")
)

server <- function(input, output, session) {
  carry_thru <- reactiveVal(list(
    fty_parameters_met = reactiveVal(TRUE),
    selected = reactiveValues(
      league_id = 95537,
      competitor_name = "britney_spears",
      opponent_name = "Only Franz"
    )
  ))

  mod_league_overview_server("league_overview_1", carry_thru)
}

shinyApp(ui, server)
