with_handoff_secret <- function(code, secret = "test-secret") {
  old <- Sys.getenv("NBA_HANDOFF_SECRET", unset = NA)
  Sys.setenv(NBA_HANDOFF_SECRET = secret)
  on.exit(
    if (is.na(old)) Sys.unsetenv("NBA_HANDOFF_SECRET") else Sys.setenv(NBA_HANDOFF_SECRET = old),
    add = TRUE
  )
  force(code)
}

fake_session <- function(query) {
  query
}

test_that("parse_handoff extracts a valid token", {
  with_handoff_secret({
    q <- nba.shiny.core::handoff_query("cus_1", "ESPN", 95537, 42, ttl = 300)
    h <- parse_handoff(fake_session(paste0("?", q)))
    expect_equal(h$platform, "ESPN")
    expect_equal(h$league_id, 95537L)
    expect_equal(h$competitor_id, 42L)
    expect_equal(h$customer_id, "cus_1")
  })
})

test_that("parse_handoff rejects missing, tampered and expired tokens", {
  with_handoff_secret({
    expect_null(parse_handoff(fake_session("")))
    expect_null(parse_handoff(fake_session("?foo=bar")))
    expect_null(parse_handoff(fake_session(
      "?customer_id=c&platform=ESPN&league_id=1&competitor_id=2&exp=9999999999&sig=deadbeef"
    )))

    expired <- nba.shiny.core::handoff_query("c", "ESPN", 1, 2, exp = 100)
    expect_null(parse_handoff(fake_session(paste0("?", expired))))
  })
})

test_that("seed_handoff sets the carry-through and marks the handoff active", {
  rv <- shiny::reactiveValues(fty_parameters_met = FALSE)
  seed_handoff(rv, list(platform = "ESPN", league_id = -1L, competitor_id = 2L, customer_id = "c"))
  expect_true(shiny::isolate(rv$handoff_active))
  expect_true(shiny::isolate(rv$fty_parameters_met))
  expect_equal(shiny::isolate(rv$platform), "ESPN")
  expect_equal(shiny::isolate(rv$customer_id), "c")
})

test_that("the league parses exactly the query the entry point builds", {
  with_handoff_secret({
    # Mirrors nba.shiny.entry::league_iframe_url(): <space>/?<handoff_query>
    url <- paste0(
      "https://shaggycamel-nba-shiny-espn-95537.hf.space/?",
      nba.shiny.core::handoff_query("cus_a24dgn8202vt", "ESPN", 95537, 25, ttl = 300)
    )
    h <- parse_handoff(sub("^[^?]*\\?", "", url))
    expect_equal(
      h,
      list(platform = "ESPN", league_id = 95537L, competitor_id = 25L, customer_id = "cus_a24dgn8202vt")
    )
  })
})
