# The greatest superset of categories
cats <- c("min", "fgm", "fga", "fg3_m", "ftm", "fta", "pts", "reb", "ast", "stl", "blk", "tov", "pf", "dd2", "td3")
cur_date <- as.Date(format(Sys.time(), tz = "America/New_York"))
# cur_date <- as.Date("2026-01-01")
cur_season <- "2025-26"
prev_season <- str_replace_all(cur_season, "\\d+", \(x) as.character(as.integer(x) - 1))

usethis::use_data(cur_date, overwrite = TRUE)
