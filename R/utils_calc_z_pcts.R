#' calc_z_scores
#'
#' @description Calculate z-scores
#'
#' @return A dataframe containing z-score calculations
#'
#' @noRd
#'
calc_z_pcts <- function(df) {
  df |>
    mutate(
      fg_pct = coalesce(fgm / fga, 0),
      ft_pct = coalesce(ftm / fta, 0),
      fg_impact = (fg_pct - (sum(fgm, na.rm = TRUE) / sum(fga, na.rm = TRUE))) * fga,
      ft_impact = (ft_pct - (sum(ftm, na.rm = TRUE) / sum(fta, na.rm = TRUE))) * fta,
      # Median z-scores: uses median & median absolute deviaton instead of mean & sd
      fg_z = (fg_impact - median(fg_impact, na.rm = TRUE)) / mad(fg_impact, na.rm = TRUE),
      ft_z = (ft_impact - median(ft_impact, na.rm = TRUE)) / mad(ft_impact, na.rm = TRUE)
    ) |>
    select(-ends_with("impact"))
}
