#' game_tbl_col_fmt
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom reactable colDef
#' @importFrom purrr map set_names
#' @importFrom stringr str_subset str_detect
game_tbl_col_fmt <- function(df, type = "player") {
  col_fmt <- map(set_names(str_subset(colnames(df), "player_", negate = TRUE)), \(x) {
    colDef(minWidth = 70, align = "center", style = function(value) {
      if (str_detect(value, "\\*") | value > 10) {
        list(background = "#ea7878ff")
      }
    })
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
