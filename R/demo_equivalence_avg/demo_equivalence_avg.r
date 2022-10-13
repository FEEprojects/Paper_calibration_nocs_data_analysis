#' This scripts computes the metrics for the different combinations possible of
#' 40 consecutive days of collocation at 2min, 1hour and 1 day temporal 
#' resolution.
#' 
#' The results are stored in the folder /output/demo_equivalence in three files:
#'  - res_2min_dates.rds
#'  - res_1h_dates.rds
#'  - res_1day_dates.rds
#'


source("R/utilities/utilities.R")
source("R/utilities/nested_models.r")
source("R/utilities/variables.r")

df_2min <- readRDS(file = DF_JOINED)
#df_2min_roll <- readRDS( file = DF_JOINED_ROLL)

output_folder <- "/output/demo_equivalence_avg/"

df_2min$sensor %>% unique()

df_2min$cutHour <- cut(df_2min$date, breaks = "1 hour")
df_2min$cutDay <- as.POSIXct(cut(df_2min$date, breaks = "1 day"))

df_1hour <- df_2min %>%
  group_by(sensor, site, cutDay, cutHour) %>%
  summarise(median_PM25 = mean(median_PM25, na.rm = T),
            PM2.5 = mean(PM2.5, na.rm = T)) %>%
  mutate(date = as.POSIXct(cutHour, tz = "UTC")) %>%
  ungroup() %>%
  dplyr::select(-cutHour)

df_1day <- df_2min %>%
  group_by(sensor, site, cutDay) %>%
  summarise(median_PM25 = mean(median_PM25, na.rm = T),
            PM2.5 = mean(PM2.5, na.rm = T)) %>%
  mutate(date = as.POSIXct(cutDay)) %>%
  ungroup()

dates <- unique(df_1day$date, tz = "UTC")

# get all the possible subsets of 40 days.
dates_list <- c()
for (i in 1:(length(dates) - 40)) {
  dates_list <- append(dates_list, list(dates[i:(i + 40)]))
}

#wrapper for lapply
my_func <-
  function(df,
           sensor_field = "median_PM25",
           reference_field = "PM2.5",
           method) {
    tmp <- df %>%
      calculate_metrics(
        sensor_field = sensor_field,
        reference_field = reference_field,
        method = method,
        limit_value = limit_value,
        rm_uncertainty = rm_uncertainty
      )
    tmp$res$startDay <- min(df$cutDay)
    tmp$res_bus$startDay <- min(df$cutDay)
    tmp
  }


# 2 min

res_2min_dates <- lapply(dates_list,
                         function(x)
                           df_2min[df_2min$cutDay %in% x, ])  %>%
  lapply(., function(x)
    my_func(
      x,
      method = "2 min",
      sensor_field = "median_PM25",
      reference_field = "PM2.5"
    )) #%>%
#bind_rows()
#
saveRDS(res_2min_dates,
        file = paste0(output_folder, "res_2min_dates.rds"))


# 1 hour

res_1h_dates <- lapply(dates_list,
                       function(x)
                         df_1hour[df_1hour$cutDay %in% x,])  %>%
  lapply(., function(x)
    my_func(x, method = "1 hour"))
saveRDS(res_1h_dates,
        file = paste0(output_folder, "res_1h_dates.rds"))

# 1 day

res_1day_dates <- lapply(dates_list,
                         function(x)
                           df_1day[df_1day$date %in% x, ]) %>%
  lapply(., function(x)
    my_func(x, method = "1 day"))
saveRDS(res_1day_dates,
        file = paste0(output_folder, "res_1day_dates.rds"))