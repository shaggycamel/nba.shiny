#' Parse and verify a league handoff token from the URL
#'
#' The entry point embeds a league container with a signed query string. When
#' present and valid, the dashboard starts already scoped to the customer's
#' manager (no login modal).
#'
#' @param query The URL query string (e.g. `session$clientData$url_search`).
#' @return A list with `platform`, `league_id`, `competitor_id`, `customer_id`,
#'   or `NULL` when the token is absent or invalid.
#' @noRd
parse_handoff <- function(query) {
  if (is.null(query) || !nzchar(query)) {
    return(NULL)
  }

  params <- parseQueryString(query)
  needed <- c("customer_id", "platform", "league_id", "competitor_id", "exp", "sig")
  if (!all(needed %in% names(params))) {
    return(NULL)
  }

  ok <- nba.shiny.core::verify_handoff_token(
    params$sig,
    customer_id = params$customer_id,
    platform = params$platform,
    league_id = params$league_id,
    competitor_id = params$competitor_id,
    exp = params$exp
  )
  if (!isTRUE(ok)) {
    return(NULL)
  }

  list(
    platform = params$platform,
    league_id = as.integer(params$league_id),
    competitor_id = as.integer(params$competitor_id),
    customer_id = params$customer_id
  )
}

#' Seed the dashboard from a verified handoff token
#'
#' Sets the same `rv_carry_thru` fields the login modal would, then marks the
#' handoff active so the modal is skipped.
#'
#' @param rv_carry_thru The carry-through `reactiveValues`.
#' @param handoff Result of [parse_handoff()].
#' @noRd
seed_handoff <- function(rv_carry_thru, handoff) {
  league_id_chr <- as.character(handoff$league_id)

  rv_carry_thru$league_id <- handoff$league_id
  rv_carry_thru$platform <- handoff$platform
  rv_carry_thru$league_name <- pluck(ls_fty_lookup, "lg_id_to_name", league_id_chr)
  rv_carry_thru$competitor_id <- handoff$competitor_id
  rv_carry_thru$competitor_name <- pluck(
    ls_fty_lookup,
    "cp_id_to_name",
    league_id_chr,
    as.character(handoff$competitor_id)
  )

  schedule <- if (league_id_chr %in% names(dfs_fty_schedule)) {
    dfs_fty_schedule[[league_id_chr]]
  } else {
    NULL
  }
  rv_carry_thru$cur_matchup_period <- if (is.null(schedule)) {
    NA_integer_
  } else {
    schedule |>
      filter(matchup_start <= cur_date, matchup_end >= cur_date) |>
      pull(matchup_period) |>
      pluck(1)
  }

  rv_carry_thru$customer_id <- handoff$customer_id
  rv_carry_thru$handoff_active <- TRUE
  rv_carry_thru$fty_parameters_met <- TRUE

  invisible(rv_carry_thru)
}
