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

# functions
source(here("R", "utils_database.R"))
source(here("R", "fct_calc_z_pcts.R"))


# generate data files
files <- list.files(here("data-raw"), pattern = "^[^_]")
files <- discard(files, \(x) str_like(x, "%h2h.R"))

# Generate Data ----------------------------------------------------------

walk(files, \(file) {
  cat(paste("\nExecuting:", file, "\n"))
  source(here("data-raw", file))
})

# DOUBLE CHECK WHAT OBJECTS NEED TO BE SAVED
