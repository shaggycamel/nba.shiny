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

# database functions
source(here("R", "utils_database.R"))


# generate data files
files <- list.files(here("data-raw"), pattern = "^[^_]")


# Generate Data ----------------------------------------------------------

walk(files, \(file) {
  source(here("data-raw", file))
})
