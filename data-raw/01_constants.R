cur_date <- as.Date(format(Sys.time(), tz = "America/New_York"))
cur_season <- "2025-26"
prev_season <- "2024-25" # THINK THERE IS A STRING FUNCTION FOR THIS???
cats <- c("min", "fgm", "fga", "fg3_m", "ftm", "fta", "pts", "reb", "ast", "stl", "blk", "tov", "pf", "dd2", "td3")


usethis::use_data(cur_date, overwrite = TRUE)
