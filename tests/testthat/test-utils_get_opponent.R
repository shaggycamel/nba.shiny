# get_opponent() reads the league's schedule to find who a competitor plays in
# a given matchup period, then resolves the display name through the id lookup.
# Both come from package data, so the tests mock them rather than depending on
# whichever season the committed data happens to hold.

fake_schedule <- function() {
  list(
    "1" = tibble(
      competitor_id = c(1L, 1L, 2L),
      matchup_period = c(1, 2, 1),
      opponent_id = c(9L, 10L, 8L)
    )
  )
}

fake_lookup <- function() {
  list(
    cp_id_to_name = list(
      "1" = c("1" = "Me", "8" = "Other", "9" = "Rival", "10" = "Later")
    )
  )
}

test_that("the opponent id and name come from the requested matchup", {
  local_pkg_data(
    dfs_fty_schedule = fake_schedule(),
    ls_fty_lookup = fake_lookup()
  )

  expect_equal(
    get_opponent(list(league_id = 1, competitor_id = 1), 1),
    list(id = 9L, name = "Rival")
  )
  expect_equal(
    get_opponent(list(league_id = 1, competitor_id = 1), 2),
    list(id = 10L, name = "Later")
  )
})

test_that("a competitor with no scheduled game returns no opponent", {
  local_pkg_data(
    dfs_fty_schedule = fake_schedule(),
    ls_fty_lookup = fake_lookup()
  )

  # Period 3 is not in the schedule, so the id vector is empty rather than an
  # integer(0) that would break the lookup.
  expect_equal(
    get_opponent(list(league_id = 1, competitor_id = 1), 3),
    list(id = NA, name = NULL)
  )
})

test_that("a bye keeps an NA id and an unresolved name", {
  schedule <- fake_schedule()
  schedule[["1"]]$opponent_id[
    schedule[["1"]]$competitor_id == 1 &
      schedule[["1"]]$matchup_period == 1
  ] <- NA_integer_

  local_pkg_data(
    dfs_fty_schedule = schedule,
    ls_fty_lookup = fake_lookup()
  )

  expect_equal(
    get_opponent(list(league_id = 1, competitor_id = 1), 1),
    list(id = NA_integer_, name = NULL)
  )
})
