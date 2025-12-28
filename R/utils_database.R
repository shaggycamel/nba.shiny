# Read only cockroach database connection\
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
db_con <- function() {
  dbConnect(
    drv = Postgres(),

    # Local
    user = "postgres",
    password = "zxc123",
    host = "localhost",
    port = "5432",
    dbname = "nba",
    options = "-c search_path=nba"

    # Cloud
    # user = "kobe_public",
    # password = "kobe123-69(.)(.)",
    # host = "nba-data-mgmt-9184.8nj.gcp-europe-west1.cockroachlabs.cloud",
    # port = "26257",
    # dbname = "nba",
    # options = "--cluster=nba-data-mgmt-9184"
  )
}
