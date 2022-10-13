
require(purrr)
source("R/utilities/utilities.R")
source("R/utilities/nested_models.r")
source("R/utilities/variables.r")

file_list <-
  list.files(
    path = "output/calibration/2weeks_restofyear/",
    pattern = "df_.*",
    full.names = T,
    recursive = T
  )
output_folder <-
  "output/calibration/2weeks_restoftheyear/"

# creates the output folder if it does not exist.
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = T)
}

#file_list
df_cal_pms <- data.frame()
df_train_pms <- data.frame()
df_cal_sps <- data.frame()
df_train_sps <- data.frame()

file <- file_list[7]
file
for (file in file_list) {
  name <- strsplit(file, "/")[[1]][9]
  if (grepl("initial", name)) {
    next
  }
  tmp <- readRDS(file)
  if (!grepl("train", name)) {
  #  message(file)
    if (grepl("SPS", name)) {
      if (dim(df_cal_sps)[1] == 0) {
      #  message("Fist sps cal")
        df_cal_sps <- tmp
      }
      else{
       # message("other sps cal")
        df_cal_sps <-
          inner_join(df_cal_sps, tmp, by = c("date", "sensor"))
      }
    }
    else{
      if (dim(df_cal_pms)[1] == 0) {
       # message("Fist pms cal")
        df_cal_pms <- tmp
      }
      else{
       # message("other pms cal")
        df_cal_pms <-
          inner_join(df_cal_pms, tmp, by = c("date", "sensor"))
      }
    }
  }
  else{
   # message("train")
    if (grepl("SPS", name)) {
      if (dim(df_train_sps)[1] == 0) {
        message("first sps train")
        df_train_sps <- tmp
      }
      else{
       # message("other sps train")
        df_train_sps <-
          inner_join(df_train_sps, tmp, by = c("date", "sensor"))
      }
    }
    else{
      if (dim(df_train_pms)[1] == 0) {
        df_train_pms <- tmp
      }
      else{
        df_train_pms <-
          inner_join(df_train_pms, tmp, by = c("date", "sensor"))
      }
    }
  }
}

df_cal_sps %>% duplicated %>% which

df_2min <- readRDS(DF_JOINED)
df_cal_sps$cutMonth <-
  as.POSIXct(cut(df_cal_sps$date, breaks = "1 month"))
df_cal_sps$cutWeek <-
  as.POSIXct(cut(df_cal_sps$date, breaks = "1 week"))

df_cal_sps_long <- df_cal_sps %>%
  pivot_longer(
    cols = starts_with("PM25_corr"),
    names_to = "method",
    values_to = "PM25"
  )

df_cal_sps_long <- df_cal_sps_long %>%
  inner_join(dplyr::select(df_2min, PM2.5, date, sensor, site),
             by = c("date", "sensor"))


df_cal_pms$cutMonth <-
  as.POSIXct(cut(df_cal_pms$date, breaks = "1 month"))
df_cal_pms$cutWeek <-
  as.POSIXct(cut(df_cal_pms$date, breaks = "1 week"))

df_cal_pms_long <- df_cal_pms %>%
  pivot_longer(
    cols = starts_with("PM25_corr"),
    names_to = "method",
    values_to = "PM25"
  )


df_cal_pms_long <- df_cal_pms_long %>%
  inner_join(dplyr::select(df_2min, PM2.5, date, sensor, site),
             by = c("date", "sensor"))

nest_metrics <-
  function(df,
           sensor_field = "PM25",
           reference_field = "PM2.5",
           ...) {
    tmp <- df %>%
      calculate_metrics(
        sensor_field = sensor_field,
        reference_field = reference_field,
        method = "nest"
      )
    tmp$res
  }

nest_metrics_bus <-
  function(df,
           sensor_field = "PM25",
           reference_field = "PM2.5",
           ...) {
    tmp <- df %>%
      calculate_metrics(
        sensor_field = sensor_field,
        reference_field = reference_field,
        method = "nest"
      )
    tmp$res_bus
  }

test <- df_cal_sps_long %>%
  ungroup() %>%
  group_by(cutMonth, method) %>%
  nest() %>%
  mutate(results = map(.x = data, .f = ~ nest_metrics(.x))) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(results), names_sep = ".")

test %>%
  saveRDS(
    paste0(
      output_folder,
      "results_ue_per_month_sps_calibration_2weeks_restofyear.rds"
    )
  )

test <- df_cal_pms_long %>%
  ungroup() %>%
  group_by(cutMonth, method) %>%
  nest() %>%
  mutate(results = map(.x = data, .f = ~ nest_metrics(.x))) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(results), names_sep = ".")

test %>%
  saveRDS(
    paste0(
      output_folder,
      "results_ue_per_month_pms_calibration_2weeks_restofyear.rds"
    )
  )


test <- df_cal_pms_long %>%
  ungroup() %>%
  group_by(cutMonth, method) %>%
  nest() %>%
  mutate(results = map(.x = data, .f = ~ nest_metrics_bus(.x))) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(results), names_sep = ".")

test %>%
  saveRDS(
    paste0(
      output_folder,
      "results_bus_per_month_pms_calibration_2weeks_restofyear.rds"
    )
  )

test <- df_cal_sps_long %>%
  ungroup() %>%
  group_by(cutMonth, method) %>%
  nest() %>%
  mutate(results = map(.x = data, .f = ~ nest_metrics_bus(.x))) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(results), names_sep = ".")

test %>%
  saveRDS(
    paste0(
      output_folder,
      "results_bus_per_month_sps_calibration_2weeks_restofyear.rds"
    )
  )


df_2min$cutMonth <-
  as.POSIXct(cut(df_2min$date, breaks = "1 month"))


tmp <- df_2min %>%
  ungroup() %>%
  group_by(cutMonth) %>%
  nest() %>%
  mutate(results = map(
    .x = data,
    .f = ~ nest_metrics(.x, sensor_field = "median_PM25", 
                        reference_field = "PM2.5")
  )) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(results), names_sep = ".")
tmp$method <- "Initial"
tmp %>%
  saveRDS(paste0(output_folder, "res_ue_per_month_initial.rds"))


tmp <- df_2min %>%
  ungroup() %>%
  group_by(cutMonth) %>%
  nest() %>%
  mutate(results = map(
    .x = data,
    .f = ~ nest_metrics_bus(.x, sensor_field = "median_PM25", 
                            reference_field = "PM2.5")
  )) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(results), names_sep = ".")
tmp$method <- "Initial"
tmp %>%
  saveRDS(paste0(output_folder, "res_bus_per_month_initial.rds"))


# -------------------------------


test <- df_cal_sps_long %>%
  ungroup() %>%
  group_by(cutWeek, method) %>%
  nest() %>%
  mutate(results = map(.x = data, .f = ~ nest_metrics(.x))) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(results), names_sep = ".")

test %>%
  saveRDS(
    paste0(
      output_folder,
      "results_ue_per_week_sps_calibration_2weeks_restofyear.rds"
    )
  )

test <- df_cal_pms_long %>%
  ungroup() %>%
  group_by(cutWeek, method) %>%
  nest() %>%
  mutate(results = map(.x = data, .f = ~ nest_metrics(.x))) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(results), names_sep = ".")

test %>%
  saveRDS(
    paste0(
      output_folder,
      "results_ue_per_week_pms_calibration_2weeks_restofyear.rds"
    )
  )


test <- df_cal_pms_long %>%
  ungroup() %>%
  group_by(cutWeek, method) %>%
  nest() %>%
  mutate(results = map(.x = data, .f = ~ nest_metrics_bus(.x))) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(results), names_sep = ".")

test %>%
  saveRDS(
    paste0(
      output_folder,
      "results_bus_per_week_pms_calibration_2weeks_restofyear.rds"
    )
  )

test <- df_cal_sps_long %>%
  ungroup() %>%
  group_by(cutWeek, method) %>%
  nest() %>%
  mutate(results = map(.x = data, .f = ~ nest_metrics_bus(.x))) %>%
  dplyr::select(-data) %>%
  unnest(cols = c(results), names_sep = ".")

test %>%
  saveRDS(
    paste0(
      output_folder,
      "results_bus_per_week_sps_calibration_2weeks_restofyear.rds"
    )
  )
