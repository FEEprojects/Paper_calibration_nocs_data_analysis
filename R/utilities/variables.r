FIDAS_PM_DATA_FILE <- "C:/Data/Fidas/promo/df_pm_2min.rds"
FIDAS_SIZE_DATA_FILE <- "C:/Data/Fidas/promo/df_distrib_2min.rds"
FIDAS_WEATHER_DATA_FILE <- "C:/Data/Fidas/promo/df_weather_2min.rds"

SENSORS_DATA_FILE_10s <- "C:/Data/nocs/202007_to_202107_nocs_raw.rds"
SENSORS_DATA_FILE <- "C:/Data/nocs/202007_to_202107_nocs.rds"


DF_JOINED <- "C:/Github/AQ_analysis_calibration/data/df_join.rds"
DF_JOINED_ROLL <- "C:/Github/AQ_analysis_calibration/data/df_join_14min_roll.rds"


rm_uncertainty <-  0.48/sqrt(2)
limit_value = 25

PLOT_OUTPUT_FOLDER <- paste0(here::here(),"/output/full_year/plots/")
