# Data generation is split into two layers:
#   1. _generate_base.R   - shared NBA data, generated once (data-raw/cache/base.rda)
#   2. _generate_league.R - league fantasy data, generated per league container
# Each layer sources _setup.R and opens its own db connection, so they can also
# be run standalone (deploy/cron.sh runs base once, then one league at a time).
source(here::here("data-raw", "_generate_base.R"))
source(here::here("data-raw", "_generate_league.R"))
