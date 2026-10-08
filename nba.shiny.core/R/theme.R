#' Shared NBA Shiny theme
#'
#' The bslib theme used by every NBA Shiny app (league dashboards and the
#' entry point), so they look identical.
#'
#' @return A `bslib::bs_theme()` object.
#' @export
nba_theme <- function() {
  bs_theme(
    version = 5,
    preset = "litera",
    primary = "#133DEF"
  )
}
