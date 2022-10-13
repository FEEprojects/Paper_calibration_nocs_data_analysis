source("R/utilities/utilities.R")
source("R/utilities/nested_models.r")
source("R/utilities/variables.r")

df_2min <- readRDS(DF_JOINED)

sensor_type = "PMS"
output_folder <-
  "output/calibration/test_2weeks/"
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = T)
}
bin_field = c(
  "median_gr03um",
  "median_gr05um",
  "median_gr10um",
  "median_gr25um",
  "median_gr50um",
  "median_gr100um"
)
sensor_field = "median_PM25"
reference_field = "PM2.5"
rh_field = "rh"

df_train <-
  df_2min[df_2min$date <= as.POSIXct("2020-07-14"), ]
df_calibration <-
  df_2min[df_2min$date > as.POSIXct("2020-07-14") &
                 df_2min$date <= as.POSIXct("2020-08-23"),]


# PMS ----------------------
sensor_type = "PMS"

bin_field = c(
  "median_gr03um",
  "median_gr05um",
  "median_gr10um",
  "median_gr25um",
  "median_gr50um",
  "median_gr100um"
)

#
method_list = c("svm",
                "svmpart",
                "svmpartrh",
                "gbm",
                "gbmrh",
                "gbmpart",
                "gbmpartrh")
calibration_scenario = "2 weeks"
df_weather_pm_train <-
  df_train[grepl("PMS", df_train$sensor) &
             !is.na(df_train[[reference_field]]),] %>%
  ungroup()


df_weather_pm_calibration <-
  df_calibration[grepl("PMS", df_calibration$sensor) &
                   !is.na(df_calibration[[reference_field]]),] %>%
  ungroup()

calibrate_ml_sensors(
  df_weather_pm_train,
  df_weather_pm_calibration,
  sensor_type = sensor_type,
  calibration_scenario = calibration_scenario,
  output_folder = output_folder,
  method_list = method_list,
  sensor_field = sensor_field,
  reference_field = reference_field,
  rh_field = rh_field,
  limit_value = limit_value,
  rm_uncertainty = rm_uncertainty,
  bin_field = bin_field
)

# get the initial state
calibrate_sensors(
  df_weather_pm_train,
  df_weather_pm_calibration,
  sensor_type = sensor_type,
  calibration_scenario = calibration_scenario,
  output_folder = output_folder,
  method_list = c("initial"),
  sensor_field = sensor_field,
  reference_field = reference_field,
  rh_field = rh_field,
  limit_value = limit_value,
  rm_uncertainty = rm_uncertainty,
  bin_field = bin_field
)
#
bin_field = c("median_n05",
              "median_n1",
              "median_n25",
              "median_n4",
              "median_n10")


# SPS ----------------------
sensor_type <- "SPS"
df_weather_pm_train <- df_train %>%
  dplyr::filter(grepl("SPS", sensor)) %>%
  dplyr::filter(sensor != "SPS030-60767820-40") %>%
  ungroup() %>%
  dplyr::filter(!is.na(.data[[reference_field]]))

df_weather_pm_calibration <- df_calibration %>%
  dplyr::filter(grepl("SPS", sensor)) %>%
  dplyr::filter(sensor != "SPS030-60767820-40") %>%
  ungroup() %>%
  dplyr::filter(!is.na(.data[[reference_field]]))

calibrate_ml_sensors(
  df_weather_pm_train,
  df_weather_pm_calibration,
  sensor_type = sensor_type,
  calibration_scenario = calibration_scenario,
  output_folder = output_folder,
  method_list = method_list,
  sensor_field = sensor_field,
  reference_field = reference_field,
  rh_field = rh_field,
  limit_value = limit_value,
  rm_uncertainty = rm_uncertainty,
  bin_field = bin_field
)
# get the initial state
calibrate_sensors(
  df_weather_pm_train,
  df_weather_pm_calibration,
  sensor_type = sensor_type,
  calibration_scenario = calibration_scenario,
  output_folder = output_folder,
  method_list = c("initial"),
  sensor_field = sensor_field,
  reference_field = reference_field,
  rh_field = rh_field,
  limit_value = limit_value,
  rm_uncertainty = rm_uncertainty,
  bin_field = bin_field
)
