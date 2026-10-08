#' resolve_matchup_selection
#'
#' @description Pick the matchup period to show after a league or competitor
#' switch. Keep the current selection when it still exists, but clamp a longer
#' league's period down to the new league's last regular matchup.
#'
#' @return A single matchup period.
#'
#' @noRd
#'
resolve_matchup_selection <- function(current, valid_periods, fallback) {
  current <- suppressWarnings(as.integer(current))
  greatest_regular <- max(valid_periods[valid_periods != 99], na.rm = TRUE)

  if (length(current) != 1L || is.na(current)) {
    fallback
  } else if (current %in% valid_periods) {
    current
  } else if (current > greatest_regular) {
    greatest_regular
  } else {
    fallback
  }
}


#' matchup_period_from_label
#'
#' @description Read the period out of a schedule-table label like
#' "3 (2025-11-03)" or "Post Fantasy".
#'
#' @return A single matchup period.
#'
#' @noRd
#'
matchup_period_from_label <- function(label) {
  ifelse(label == "Post Fantasy", 99L, suppressWarnings(as.integer(str_extract(label, "^\\d+"))))
}
