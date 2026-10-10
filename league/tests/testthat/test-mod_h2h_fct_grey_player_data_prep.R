# grey_player_data_prep() works out which days a player should be greyed in the
# h2h table: the head of the matchup before they were added, and the tail after
# they were dropped. It reads the current roster from package data, so the tests
# mock that and drive it with a hand-built df_base.

grey_fixtures <- function() {
  start <- cur_date - 3

  df_base <- tibble(
    player_id = c(10L, 10L, 11L, 11L, 12L, 12L, 20L, 21L, 30L),
    game_date = as.Date(c(
      start,
      start + 1,
      start + 1,
      cur_date,
      start,
      cur_date,
      start,
      start,
      start
    )),
    tense = "past"
  )

  roster <- tibble(
    competitor_id = c(1L, 1L, 1L, 1L, 2L, 2L, 3L),
    matchup_period = c(5L, 5L, 5L, 5L, 5L, 6L, 5L),
    player_id = c(10L, 11L, 12L, 12L, 20L, 21L, 30L),
    assigned_date = as.Date(c(
      start,
      cur_date,
      start,
      cur_date,
      start,
      start,
      start
    ))
  ) |>
    mutate(matchup_start = start, matchup_end = cur_date + 3)

  list(df_base = df_base, roster = roster)
}

grey_prep <- function(input, df_base, rv_carry_thru, opponent) {
  local_pkg_data(dfs_fty_roster = list("1" = grey_fixtures()$roster))
  grey_player_data_prep(input, df_base, rv_carry_thru, opponent)
}

grey_opponent <- function() list(id = 2L, name = "Rival")

test_that("a player rostered the whole matchup with the latest assignment is never grey", {
  out <- grey_prep(
    list(matchup = 5L),
    grey_fixtures()$df_base,
    list(league_id = 1, competitor_id = 1),
    grey_opponent
  )

  # Player 12 has been held since the matchup start and its last assignment is
  # the latest, so there are no grey cells for it.
  expect_false(12L %in% out$player_id)
})

test_that("a player added mid-matchup is grey from their first game", {
  out <- grey_prep(
    list(matchup = 5L),
    grey_fixtures()$df_base,
    list(league_id = 1, competitor_id = 1),
    grey_opponent
  )
  added <- filter(out, player_id == 11L)

  expect_equal(added$min_grey_date, cur_date - 2)
  expect_true(is.na(added$max_grey_date))
})

test_that("a player dropped mid-matchup is grey to their last game", {
  out <- grey_prep(
    list(matchup = 5L),
    grey_fixtures()$df_base,
    list(league_id = 1, competitor_id = 1),
    grey_opponent
  )
  dropped <- filter(out, player_id == 10L)

  expect_true(is.na(dropped$min_grey_date))
  expect_equal(dropped$max_grey_date, cur_date - 2)
})

test_that("only the two opponents' current-matchup players are considered", {
  out <- grey_prep(
    list(matchup = 5L),
    grey_fixtures()$df_base,
    list(league_id = 1, competitor_id = 1),
    grey_opponent
  )

  # Player 30 belongs to another competitor and 21 is on the opponent's next
  # matchup, so neither gets a grey row.
  expect_false(any(c(30L, 21L) %in% out$player_id))
})

test_that("the result carries only the player's grey boundaries", {
  out <- grey_prep(
    list(matchup = 5L),
    grey_fixtures()$df_base,
    list(league_id = 1, competitor_id = 1),
    grey_opponent
  )

  expect_named(out, c("player_id", "min_grey_date", "max_grey_date"))
})
