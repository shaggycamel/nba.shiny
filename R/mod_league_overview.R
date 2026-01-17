#' league_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_league_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      tags$head(tags$style(HTML(".selectize-dropdown-content{min-width: 100%; box-sizing: border-box;}"))),
      sidebar = sidebar(
        selectInput(
          ns("fty_lg_ov_cat"),
          "Category",
          choices = character(0)
        ),
        switchInput(ns("fty_lg_ov_rank_toggle"), value = TRUE, onLabel = "Rank", offLabel = "Value", size = "small"),
        switchInput(ns("fty_lg_ov_cum_toggle"), value = TRUE, onLabel = "W2W", offLabel = "Cum", size = "small"),
        checkboxInput(ns("fty_lg_ov_just_h2h"), "Just H2H")
      ),
      card(full_screen = TRUE, plotlyOutput(ns("fty_lo_plt"))),
      fillable = TRUE
    )
  )
}


#' league_overview Server Functions
#'
#' @noRd
mod_league_overview_server <- function(id, carry_thru) {
  moduleServer(id, function(input, output, session) {
    # ns <- session$ns # probs delete

    # Update categories ------------------------------------------------------

    observe({
      req(carry_thru()$fty_parameters_met())
      updateSelectInput(
        session = session,
        inputId = "fty_lg_ov_cat",
        choices = pluck(ls_lov_lg_cats, as.character(carry_thru()$selected$league_id))
      )
    }) |>
      bindEvent(carry_thru()$fty_parameters_met()) # Bind event of when league is swapped too

    # Data prep --------------------------------------------------------------

    df_lo <- reactive(pluck(df_league_overview, as.character(carry_thru()$selected$league_id)))
    df_lo_pt <- reactive(filter(df_lo(), as.integer(matchup_sigmoid) == matchup_sigmoid))

    # Plot -------------------------------------------------------------------

    output$fty_lo_plt <- renderPlotly({
      req(carry_thru()$fty_parameters_met())

      plot_col <- input$fty_lg_ov_cat
      if (input$fty_lg_ov_rank_toggle) {
        plot_col <- str_c(plot_col, "_rank")
      }

      plt <- if (input$fty_lg_ov_cum_toggle) {
        df_lo() |>
          ggplot(aes(x = matchup_sigmoid, y = !!sym(plot_col), colour = competitor_name)) +
          geom_line(linewidth = 0.5) +
          geom_point(data = df_lo_pt(), size = 2) +
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

      plt <- ggplotly(plt) |>
        style(hoverinfo = "none", traces = 0:length(unique(df_lo()$competitor_id))) |>
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) |>
        rangeslider(
          start = ifelse(!input$fty_lg_ov_cum_toggle, 1, max(df_lo_pt()$matchup) - 5.1),
          end = max(df_lo_pt()$matchup) + 0.1,
          range = list(min(df_lo_pt()$matchup) - 0.2, max(df_lo_pt()$matchup) + 0.2)
        ) |>
        config(displayModeBar = FALSE)

      # TO DO:
      # Waiting on df_fty_schedule
      # if (input$fty_lg_ov_just_h2h) {
      #   plt <- {
      #     c_id <-  ls_fty_name_to_cid[input$fty_competitor_select]
      #     o_id <- df_fty_schedule |>
      #       filter(competitor_id == c_id, matchup_period == cur_matchup) |>
      #       pull(opponent_id)

      #     ts <- map_int(1:length(plt$x$data), \(x) unlist(ls_fty_name_to_cid[pluck(plt$x$data, x, "name")]))
      #     ts_vis <- c(which(ts == c_id), which(ts == o_id))

      #     style(plt, visible = "legendonly", traces = setdiff(1:length(plt$x$data), ts_vis))
      #   }
      # }
      plt
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
# load("data/df_league_overview.rda")
# load("data/ls_lov_lg_cats.rda")

# ui <- page_fluid(
#   mod_league_overview_ui("league_overview_1")
# )

# server <- function(input, output, session) {
#   carry_thru <- reactiveVal(list(
#     fty_parameters_met = reactiveVal(TRUE),
#     selected = reactiveValues(league_id = 95537)
#   ))

#   mod_league_overview_server("league_overview_1", carry_thru)
# }

# shinyApp(ui, server)
