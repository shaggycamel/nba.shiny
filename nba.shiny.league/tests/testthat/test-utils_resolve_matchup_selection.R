valid <- 1:19
valid_with_post <- c(1:19, 99)

test_that("a selection that exists in the new league is kept", {
  expect_equal(resolve_matchup_selection("15", valid, 99), 15)
  expect_equal(resolve_matchup_selection(19, valid, 99), 19)
})

test_that("post season is always kept", {
  expect_equal(resolve_matchup_selection("99", valid_with_post, 19), 99)
})

test_that("a selection past the league's last regular matchup is clamped", {
  # 22 from a longer league, new league ends at 19.
  expect_equal(resolve_matchup_selection("22", valid, 99), 19)
  expect_equal(resolve_matchup_selection(150, valid_with_post, 99), 19)
})

test_that("no usable selection falls back to the league's current matchup", {
  expect_equal(resolve_matchup_selection(NULL, valid, 99), 99)
  expect_equal(resolve_matchup_selection("0", valid, 99), 99)
  expect_equal(resolve_matchup_selection(NA, valid, 99), 99)
})

test_that("any value past the last regular matchup clamps to it", {
  expect_equal(resolve_matchup_selection("20", valid, 99), 19)
  expect_equal(resolve_matchup_selection("21", valid, 99), 19)
})

test_that("matchup_period_from_label reads the schedule-table labels", {
  expect_equal(matchup_period_from_label("3 (2025-11-03)"), 3L)
  expect_equal(matchup_period_from_label("22 (2026-03-30)"), 22L)
  expect_equal(matchup_period_from_label("Post Fantasy"), 99L)
  expect_true(is.na(matchup_period_from_label("")))
})

test_that("the schedule-table selection clamps using parsed labels", {
  labels <- c("1 (2025-10-20)", "2 (2025-10-27)", paste0(3:19, " (x)"), "Post Fantasy")
  periods <- matchup_period_from_label(labels)

  expect_equal(resolve_matchup_selection(matchup_period_from_label("15 (x)"), periods, 99), 15L)
  expect_equal(resolve_matchup_selection(matchup_period_from_label("22 (x)"), periods, 99), 19L)
  expect_equal(resolve_matchup_selection(matchup_period_from_label("Post Fantasy"), periods, 19), 99L)
})
