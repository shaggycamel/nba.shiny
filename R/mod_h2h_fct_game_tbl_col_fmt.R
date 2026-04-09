#' game_tbl_col_fmt
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
game_tbl_col_fmt <- function(df, dt, mup_end, type = "player") {
  col_fmt <- map(set_names(str_subset(colnames(df), "\\/")), \(x) {
    nm <- str_split_1(x, " ")
    colDef(
      minWidth = 70,
      align = "center",
      header = tags$span(nm[1], tags$br(), nm[2]),
      style = function(value, index) {
        if (
          type == "player" &&
            (parse_date_time(x, orders = "%a (%d/%m)") < coalesce(df$min_grey_date[index], as.Date("1000-01-01")) |
              parse_date_time(x, orders = "%a (%d/%m)") > coalesce(df$max_grey_date[index], as.Date("3000-01-01")))
          # tryCatch(
          #   parse_date_time(x, orders = "%a (%d/%m)") > coalesce(df$grey_date[index], as.Date("2999-01-01")),
          #   warning = \(w) FALSE
          # )
        ) {
          list(background = "#d7d7d5", color = "#d7d7d5")
        } else if (str_detect(value, "\\*") | value > 10) {
          list(background = "#ea7878ff")
        } else if (tryCatch(parse_date_time(x, orders = "%a (%d/%m)") == dt, warning = \(w) FALSE)) {
          list(background = "#f1e78e94")
        } else if (tryCatch(parse_date_time(x, orders = "%a (%d/%m)") > mup_end, warning = \(w) FALSE)) {
          list(background = "#eee5ff94")
        }
      }
    )
  })
  col_fmt[["games_remaining"]] <- colDef(
    minWidth = 70,
    align = "center",
    header = tags$span("Games", tags$br(), "Remaining"),
    style = list(background = "#96e5cbeb")
  )
  col_fmt[["min_grey_date"]] <- colDef(show = FALSE)
  col_fmt[["max_grey_date"]] <- colDef(show = FALSE)
  col_fmt[["competitor"]] <- colDef(show = FALSE)
  col_fmt[["player_id"]] <- colDef(show = FALSE)
  col_fmt[["player_name"]] <- colDef(
    show = ifelse(type == "sum", FALSE, TRUE),
    name = "",
    align = "left",
    minWidth = 115,
    sticky = "left",
    style = list(
      whiteSpace = "nowrap",
      overflow = "hidden",
      textOverflow = "ellipsis"
    )
  )
  col_fmt[["player_team"]] <- colDef(
    name = "",
    # align = ifelse(type == "sum", "center", "left"),
    minWidth = ifelse(type == "sum", 65 + 115, 65),
    sticky = "left",
    style = list(
      whiteSpace = "nowrap",
      overflow = "hidden",
      textOverflow = "ellipsis"
    )
  )

  col_fmt
}
