# db_connect() opens a connection to the read-only NBA database. Tests can't
# assume a Postgres/Cockroach instance is running, so they attempt the
# connection and skip when it isn't reachable.

test_that("db_connect opens a valid read-only connection when the database is up", {
  con <- tryCatch(db_connect("postgres"), error = function(e) NULL)
  skip_if(is.null(con), "no local NBA database available")

  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_s4_class(con, "PqConnection")
  expect_true(DBI::dbIsValid(con))
})
