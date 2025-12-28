df_fty_base <-
  nba.dataRub::dh_getQuery(db_con, "SELECT * FROM fty.fty_base_vw") |>
  # tbl(db_con, "fty.fty_base_vw") |>
  filter(season == cur_season) |>
  arrange(league_id)

df_fty_cats <-
  nba.dataRub::dh_getQuery(db_con, "SELECT * FROM fty.fty_categories_vw") |>
  # tbl(db_con, "fty.fty_categories_vw") |>
  filter(season == cur_season | is.na(league_id))

ls_fty_lookup <- list(
  "name_to_id" = as.list(deframe(distinct(df_fty_base, league_name, league_id))),
  "id_to_name" = as.list(deframe(distinct(df_fty_base, league_id, league_name))),
  "id_to_platform" = as.list(deframe(distinct(df_fty_base, league_id, platform)))
)

usethis::use_data(
  df_fty_base,
  df_fty_cats,
  ls_fty_lookup,
  overwrite = TRUE
)
