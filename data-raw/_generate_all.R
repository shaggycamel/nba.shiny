library(DBI)
library(RPostgres)
library(dbplyr)
library(here)
library(purrr)
library(glue)
library(tibble)
library(dplyr)
library(stringr)
library(tidyselect)
library(tidyr)
library(forcats)
library(lubridate)
library(pracma)


# functions
source(here("R", "utils_database.R"))
source(here("R", "utils_calc_z_pcts.R"))

# generate data files
files <- list.files(here("data-raw"), pattern = "^[^_]")
# files <- discard(files, \(x) str_detect(x, "h2h"))

# Generate Data ----------------------------------------------------------

cus_id <- Sys.getenv("CUSTOMER_ID", unset = NA)
if (is.na(cus_id) || cus_id == "") {
  if (interactive()) {
    cus_id <- "cus_a24dgn8202vt"
  } else {
    stop("CUSTOMER_ID environment variable not set — refusing to generate data")
  }
}

walk(files, \(file) {
  cat(paste("\nExecuting:", file, "\n"))
  source(here("data-raw", file))
})
