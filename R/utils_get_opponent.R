#' get_opponent
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_opponent <- function(rv_carry_thru, mup) {
  id <- dfs_fty_schedule |>
    pluck(as.character(rv_carry_thru$league_id)) |>
    filter(competitor_id == rv_carry_thru$competitor_id, matchup_period == mup) |>
    pull(opponent_id)

  name <- pluck(
    ls_fty_lookup,
    "cp_id_to_name",
    as.character(rv_carry_thru$league_id),
    as.character(id)
  )

  list("id" = id, "name" = name)
}
