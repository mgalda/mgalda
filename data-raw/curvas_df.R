#############################
#load_all()
hfrec_libraries()


curvas_df <- read_rds("data-raw/curvas.rds")


usethis::use_data(curvas_df, overwrite = TRUE)
