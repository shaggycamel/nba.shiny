#' Build a database connection from a credentials INI section
#'
#' Reads connection details from an INI file (one section per environment) so no
#' secrets live in source.
#'
#' @param section Section of the credentials file to use. Defaults to the
#'   `NBA_DB_SECTION` environment variable, or `"cockroach-read"`.
#' @param file Path to the credentials INI. Defaults to the
#'   `NBA_DB_CREDENTIALS` environment variable, or
#'   `~/.config/scs_hub_credentials.ini`.
#'
#' @return A `DBIConnection`.
#' @export
db_connect <- function(
  section = Sys.getenv("NBA_DB_SECTION", "cockroach-read"),
  file = Sys.getenv("NBA_DB_CREDENTIALS", "~/.config/scs_hub_credentials.ini")
) {
  file <- path.expand(file)
  if (!file.exists(file)) {
    stop("Credentials file not found: ", file, call. = FALSE)
  }

  creds <- ini::read.ini(file)[[section]]
  if (is.null(creds)) {
    stop("Section '", section, "' not found in ", file, call. = FALSE)
  }

  dbConnect(
    drv = Postgres(),
    user = creds$user,
    password = creds$password,
    host = creds$host,
    port = creds$port,
    dbname = creds$database
  )
}
