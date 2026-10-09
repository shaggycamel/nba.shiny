entry_db <- function() {
  tryCatch(nba.shiny.core::db_connect("postgres"), error = function(e) NULL)
}

test_that("get_customer_leagues returns the customer's leagues with names", {
  con <- entry_db()
  skip_if(is.null(con), "no local postgres available")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  df <- get_customer_leagues(con, "cus_a24dgn8202vt", "2025-26")
  expect_gt(nrow(df), 0L)
  expect_true(all(c("platform", "league_id", "league_name", "competitor_id") %in% names(df)))
  expect_false(any(is.na(df$league_name)))
  expect_false(any(is.na(df$competitor_id)))
})

test_that("get_league_competitors returns the league's managers", {
  con <- entry_db()
  skip_if(is.null(con), "no local postgres available")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  df <- get_league_competitors(con, "ESPN", 95537, "2025-26")
  expect_gt(nrow(df), 0L)
  expect_true(all(c("competitor_id", "competitor_name") %in% names(df)))
})

test_that("get_customer_name falls back sensibly", {
  con <- entry_db()
  skip_if(is.null(con), "no local postgres available")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  nm <- get_customer_name(con, "cus_a24dgn8202vt")
  expect_type(nm, "character")
  expect_equal(nchar(nm) > 0, TRUE)
  # unknown customer falls back to the id
  expect_equal(get_customer_name(con, "does-not-exist"), "does-not-exist")
})

test_that("check_credentials verifies a stored bcrypt hash", {
  con <- entry_db()
  skip_if(is.null(con), "no local postgres available")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cleanup <- function() {
    try(DBI::dbExecute(con, "delete from fty.customer where customer_id = '__test_customer__'"), silent = TRUE)
  }
  cleanup()
  on.exit(cleanup(), add = TRUE)

  DBI::dbExecute(
    con,
    sprintf(
      "insert into fty.customer (customer_id, name, email, password_hash)
       values ('__test_customer__', 'Test User', '__test__@example.com', %s)",
      DBI::dbQuoteString(con, nba.shiny.core::hash_password("hunter2", cost = 4L))
    )
  )

  expect_equal(check_credentials(con, "__test__@example.com", "hunter2"), "__test_customer__")
  expect_true(is.na(check_credentials(con, "__test__@example.com", "wrong")))
  expect_true(is.na(check_credentials(con, "nobody@example.com", "hunter2")))
})
