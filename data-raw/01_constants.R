cur_date <- strptime(Sys.time(), "%Y", tz = "NZ")
cur_season <- "2025-26"
prev_season <- "2024-25" # THINK THERE IS A STRING FUNCTION FOR THIS???

usethis::use_data(
  cur_date,
  cur_season,
  prev_season,
  overwrite = TRUE
)
