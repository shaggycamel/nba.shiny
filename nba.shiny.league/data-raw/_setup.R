# Shared generation setup ---------------------------------------------------
# Packages required by the data-raw scripts. Sourced by both the base and league
# layers so each can be run standalone (as cron.sh does).

suppressPackageStartupMessages({
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
})
