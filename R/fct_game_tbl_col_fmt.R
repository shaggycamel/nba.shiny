#' game_tbl_col_fmt
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
game_tbl_col_fmt <- function(df, type = "player") {
  col_fmt <- map(set_names(str_subset(colnames(df), "\\/")), \(x) {
    nm <- str_split_1(x, " ")
    colDef(
      minWidth = 70,
      align = "center",
      header = tags$span(nm[1], tags$br(), nm[2]),
      style = function(value) {
        if (str_detect(value, "\\*") | value > 10) {
          list(background = "#ea7878ff")
        } else if (tryCatch(parse_date_time(x, orders = "%a (%d/%m)") == cur_date, warning = \(w) FALSE)) {
          list(background = "#f1e78e94")
        }
      }
    )
  })
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
