#' h2h UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
        card(full_screen = TRUE, min_height = 200, max_height = 650, DTOutput(ns("game_table")))
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
        choices = sort(unique((dfs_fty_schedule[[as.character(carry_thru()$selected$league_id)]])$matchup_period)),
        selected = carry_thru()$selected$cur_matchup_period
      )
      # updateSelectInput(session, "log_config", choices = ls_log_config)
    }) |>
      bindEvent(carry_thru()$fty_parameters_met())

    # Data prep --------------------------------------------------------------

  #   opponent_id <- reactive({
  #     pluck(dfs_fty_schedule, as.character(carry_thru()$selected$league_id)) |>
  #       filter(
  #         competitor_id == as.numeric(input$competitor),
  #         matchup_period == input$matchup
  #       ) |>
  #       pull(opponent_id)
  #   })

  #   df_prep <- reactive({
  #     req(carry_thru()$fty_parameters_met())

  #     df_today <- pluck(dfs_fty_roster, as.character(carry_thru()$selected$league_id)) |>
  #       filter(assigned_date == cur_date - ddays(3)) |>
  #       mutate(dow = lubridate::wday(assigned_date, week_start = 1), .after = assigned_date) |>
  #       left_join(
  #         select(df_nba_schedule, team, game_date, scheduled_to_play),
  #         by = join_by(player_team == team, assigned_date == game_date)
  #       ) |>
  #       rename(game_date = assigned_date) |>
  #       bind_rows(
  #         filter(df_player_box_score, player_name %in% input$add_player) |>
  #           slice_max(order_by = game_date, by = player_name) |>
  #           select(
  #             player_id,
  #             player_fantasy_id := str_c(str_to_lower(carry_thru()$selected$platform), "_id"),
  #             player_name,
  #             player_team = team_slug
  #           ) |>
  #           mutate(season = cur_season, competitor_id = as.numeric(input$competitor))
  #       ) |>
  #       select(-c(season_type, begin_date, end_date))

  #     df_future <- distinct(df_nba_schedule, game_date) |>
  #       filter(game_date > cur_date) |>
  #       cross_join(tibble(
  #         competitor_id = unlist(
  #           pluck(ls_fty_lookup, "cp_name_to_id", as.character(carry_thru()$selected$league_id)),
  #           use.names = FALSE
  #         )
  #       )) |>
  #       left_join(
  #         select(
  #           df_today,
  #           season,
  #           platform,
  #           # league_id,
  #           competitor_id,
  #           player_fantasy_id,
  #           player_id,
  #           player_name,
  #           player_team,
  #           player_injury_status
  #         ),
  #         by = join_by(competitor_id),
  #         relationship = "many-to-many"
  #       ) |>
  #       left_join(
  #         select(df_nba_schedule, game_date, team, scheduled_to_play),
  #         by = join_by(game_date, player_team == team)
  #       ) |>
  #       left_join(
  #         pluck(dfs_fty_schedule, as.character(carry_thru()$selected$league_id)) |>
  #           select(starts_with("matchup"), competitor_id, opponent_id),
  #         by = join_by(competitor_id, between(game_date, matchup_start, matchup_end))
  #       ) |>
  #       mutate(dow = lubridate::wday(game_date, week_start = 1)) |>
  #       select(any_of(colnames(pluck(dfs_past, as.character(carry_thru()$selected$league_id)))))

  #     # COMBINE
  #     df_h2h <- bind_rows(
  #       list(
  #         past = pluck(dfs_past, as.character(carry_thru()$selected$league_id)),
  #         today = df_today,
  #         future = df_future
  #       ),
  #       .id = "origin"
  #     ) |>
  #       left_join(
  #         select(pluck(df_rolling_stats, input$window), -c(espn_id, yahoo_id, player_name, team_slug)),
  #         by = join_by(player_id, game_date)
  #       )

  #     # FROM TOMORROW TWEAKING
  #     if (input$future_only) {
  #       df_h2h <- filter(df_h2h, origin != "past")
  #     }
  #     if (input$future_from_tomorrow) {
  #       df_h2h |>
  #         anti_join(
  #           filter(
  #             df_h2h,
  #             competitor_id == as.numeric(input$competitor),
  #             player_name %in% input$add_player,
  #             origin == "today"
  #           ),
  #           by = join_by(competitor_id, player_id, game_date)
  #         ) |>
  #         anti_join(
  #           filter(
  #             df_h2h,
  #             competitor_id == as.numeric(input$competitor),
  #             player_name %in% input$ex_player,
  #             origin == "future"
  #           ),
  #           by = join_by(competitor_id, player_id, game_date)
  #         ) |>
  #         mutate(origin = if_else(origin == "today", "past", origin))
  #     } else {
  #       df_h2h |>
  #         anti_join(
  #           filter(
  #             df_h2h,
  #             competitor_id == as.numeric(input$competitor),
  #             player_name %in% input$ex_player,
  #             origin != "past"
  #           ),
  #           by = join_by(competitor_id, player_id, game_date)
  #         )
  #     }
  #   })

  #   # Game Table -------------------------------------------------------------

  #   output$game_table <- renderDT({
  #     req(fty_parameters_met())

  #     if (input$matchup < cur_matchup & input$future_only) {
  #       datatable(
  #         as.data.frame("Future only dumbass..."),
  #         rownames = FALSE,
  #         colnames = "",
  #         options = lst(dom = "t", paging = FALSE),
  #       )
  #     } else {
  #       df_h2h() |>
  #         filter(competitor_id %in% c(as.numeric(input$competitor), opp_id), matchup_period == input$matchup) |>
  #         mutate(
  #           inj_status = case_when(
  #             scheduled_to_play == 1 &
  #               str_detect(player_injury_status, "^O|INJ") ~
  #               "1*",
  #             scheduled_to_play == 1 ~ "1",
  #             .default = NA_character_
  #           )
  #         ) |>
  #         arrange(game_date) |>
  #         pivot_wider(
  #           id_cols = c(competitor_id, opponent_id, player_team, player_name),
  #           names_from = game_date,
  #           values_from = inj_status
  #         ) |>
  #         (\(df) {
  #           inner_func <- function(x, nm) {
  #             filter(x, competitor_id == nm) |>
  #               mutate(player_team = "Total", player_name = str_trim(nm)) |>
  #               summarise(
  #                 across(starts_with("20"), \(x) as.character(sum(x == "1", na.rm = TRUE))),
  #                 .by = c(player_team, player_name)
  #               )
  #           }

  #           bind_rows(
  #             inner_func(df, opp_id),
  #             inner_func(df, as.numeric(input$competitor)),
  #             setNames(as.data.frame(matrix(rep(NA, length(colnames(df))), nrow = 1)), colnames(df)),
  #             select(
  #               filter(df, competitor_id == as.numeric(input$competitor)),
  #               starts_with(c("player", "20"))
  #             ) |>
  #               arrange(player_name) |>
  #               mutate(across(starts_with("20"), \(x) as.character(x)))
  #           )
  #         })() |>
  #         select(-starts_with(c("competitor", "opponent"))) |>
  #         (\(df) {
  #           Ttl = as.data.frame(t(df)) |>
  #             mutate(across(everything(), \(x) {
  #               ifelse(is.na(as.numeric(x)) | as.numeric(x) <= 10, as.numeric(x), 10)
  #             })) |>
  #             summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |>
  #             t()

  #           mutate(df, Total = Ttl)
  #         })() |>
  #         mutate(Total = if_else(Total == 0 & is.na(player_team), NA, Total)) |>
  #         mutate(matchup_period = as.numeric(input$matchup)) |>
  #         left_join(
  #           select(df_matchup_game_count, matchup_period, team, following_matchup_period_games),
  #           by = join_by(player_team == team, matchup_period)
  #         ) |>
  #         select(-matchup_period, following_matchup_period_games) |>
  #         distinct()

  #       df_matchup_game_count_tbl <<- df_matchup_game_count |>
  #         select(
  #           starts_with("player"),
  #           all_of(sort(str_subset(colnames(df_matchup_game_count), "\\d"))),
  #           Total,
  #           `Next Matchup Period` = following_matchup_period_games,
  #           Team = player_team,
  #           Player = player_name
  #         ) |>
  #         rename_with(.fn = \(x) format(as.Date(x), "%a (%d/%m)"), .cols = starts_with("20")) |>
  #         mutate(
  #           Player = str_replace_all(
  #             Player,
  #             setNames(
  #               unlist(ls_fty_cid_to_name),
  #               map_chr(names(ls_fty_cid_to_name), \(x) paste0("^", x, "$"))
  #             )
  #           )
  #         )

  #       max_game_count <- max(df_matchup_game_count_tbl$Total, na.rm = TRUE)
  #       min_next_matchup_game_count <- min(df_matchup_game_count_tbl$`Next Matchup Period`, na.rm = TRUE)

  #       tibble::rowid_to_column(df_matchup_game_count_tbl) |>
  #         datatable(
  #           rownames = FALSE,
  #           escape = FALSE,
  #           style = "default",
  #           options = lst(
  #             dom = "t",
  #             paging = FALSE,
  #             ordering = FALSE,
  #             columnDefs = list(list(visible = FALSE, targets = "rowid")),
  #             initComplete = JS(
  #               "function(settings, json) {",
  #               "$(this.api().table().header()).css({'background-color': 'blue', 'color': 'white'});",
  #               "}"
  #             )
  #           )
  #         ) |>
  #         formatStyle(colnames(df_matchup_game_count_tbl), border = "1px solid #000") |>
  #         formatStyle("rowid", target = "row", backgroundColor = styleEqual(3, "grey")) |>
  #         formatStyle("Total", target = "cell", backgroundColor = styleEqual(max_game_count, "lightgreen")) |>
  #         formatStyle(c("Team", "Player"), backgroundColor = "azure") |>
  #         formatStyle(
  #           "Next Matchup Period",
  #           target = "cell",
  #           backgroundColor = styleEqual(min_next_matchup_game_count, "#FFBBFF")
  #         ) |>
  #         (\(dt) {
  #           cols <- str_subset(colnames(df_matchup_game_count_tbl), "\\(")
  #           for (col in cols) {
  #             dt <- formatStyle(
  #               dt,
  #               columns = col,
  #               target = "cell",
  #               backgroundColor = styleInterval(10, c(NA, "pink"))
  #             ) |>
  #               formatStyle(columns = col, target = "cell", backgroundColor = styleEqual("1*", "pink"))
  #           }

  #           if (format(cur_date, "%a (%d/%m)") %in% cols) {
  #             dt <- formatStyle(
  #               dt,
  #               format(cur_date, "%a (%d/%m)"),
  #               target = "cell",
  #               backgroundColor = styleEqual(
  #                 "1*",
  #                 "pink",
  #                 default = "lightyellow"
  #               )
  #             )
  #           }
  #           if (length(input$hl_player) > 0) {
  #             dt <- formatStyle(
  #               dt,
  #               "Player",
  #               target = "row",
  #               backgroundColor = styleEqual(input$hl_player, rep("#54FF9F", length(input$hl_player)))
  #             )
  #           }

  #           dt
  #         })()
  #     }
  #   })
  # })
}

## To be copied in the UI
# mod_h2h_ui("h2h_1")

## To be copied in the server
# mod_h2h_server("h2h_1")

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(plotly)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
load("data/ls_fty_lookup.rda")
load("data/dfs_fty_schedule.rda")

ui <- page_fluid(
  mod_h2h_ui("h2h_1")
)

server <- function(input, output, session) {
  carry_thru <- reactiveVal(list(
    fty_parameters_met = reactiveVal(TRUE),
    selected = reactiveValues(
      platform = "ESPN",
      league_id = 95537,
      competitor_id = 25,
      competitor_name = "britney_spears",
      cur_matchup_period = 13
    )
  ))

  mod_h2h_server("h2h_1", carry_thru)
}

shinyApp(ui, server)
