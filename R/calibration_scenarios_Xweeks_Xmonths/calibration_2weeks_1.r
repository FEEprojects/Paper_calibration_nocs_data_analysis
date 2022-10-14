source("R/utilities/utilities.R")
source("R/utilities/nested_models.r")
source("R/utilities/variables.r")

df_2min_roll <- readRDS(DF_JOINED)



output_folder <- "output/calibration/scenarios/2weeks_8weeks/"
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = T)
}


sensor_field = "median_PM25"
reference_field = "PM2.5"
rh_field = "rh"
method_list = c(
  "initial",
  "lr",
  "ols",
  "mlr1",
  "koehlermass",
  "koehlersize",
  "laulainen",
  "rlmhuber",
  "rlmrh",
  "rlmpart",
  "rlmpartrh",
  "lmpart",
  "lmpartrh"
)
#method_list = c("ols")
#method_list <- c()
#method_list = c("koehlermassrh85")
df_2min_roll$cutDay <-
  as.POSIXct(cut(df_2min_roll$date, breaks = "1 day"))
dates <- unique(df_2min_roll$cutDay)
#method_list = c("rlmpart")

dates_list <- c()
for (i in 1:(length(dates) - 70)) {
  dates_list <- append(dates_list, list(dates[i:(i + 69)]))
}

require(lubridate)
#wrapper for lapply
my_func <-
  function(df,
           sensor_field = "median_PM25",
           reference_field = "PM2.5") {
    dates_list_ft <- unique(df$cutDay)
    
    dates_list_train <-
      dates_list_ft[1:14] #First 14 days for training
    dates_list_cal <-
      dates_list_ft[15:length(dates_list_ft)] # Next 8 weeks
    
    df_train <- df[df$cutDay %in% dates_list_train, ]
    df_calibration <- df[df$cutDay %in% dates_list_cal, ]
    
    sensor_type = "PMS"
    bin_field = c(
      "median_gr03um",
      "median_gr05um",
      "median_gr10um",
      "median_gr25um",
      "median_gr50um",
      "median_gr100um"
    )
    calibration_scenario <-
      paste0(substr(min(df$date), 1, 10), " Test")
    df_weather_pm_train <-
      df_train[grepl("PMS", df_train$sensor),] %>%
      ungroup()
    
    df_weather_pm_calibration <-
      df_calibration[grepl("PMS", df_calibration$sensor),] %>%
      ungroup()
    message("Calibrate PMS")
    calibrate_sensors(
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
    
    bin_field = c("median_n05",
                  "median_n1",
                  "median_n25",
                  "median_n4",
                  "median_n10")
    sensor_type <- "SPS"
    df_weather_pm_train <-
      df_train[grepl("SPS", df_train$sensor),] %>%
      ungroup()
    
    df_weather_pm_calibration <-
      df_calibration[grepl("SPS", df_calibration$sensor),] %>%
      ungroup()
    message("Calibrate SPS")
    calibrate_sensors(
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
  }


# 2 min

lapply(dates_list[21:40],
       function (x)
         df_2min_roll[df_2min_roll$cutDay %in% x,]) %>%
  lapply(., function(x)
    my_func(x, sensor_field = sensor_field,
            reference_field = reference_field)) 

