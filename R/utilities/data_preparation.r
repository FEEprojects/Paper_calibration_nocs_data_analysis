#' Prepare the data for processing

source("variables.r")
source("utilities.r")

df_sensors <- readRDS(SENSORS_DATA_FILE)
df_fidas <- readRDS(FIDAS_PM_DATA_FILE)
df_weather <- readRDS(FIDAS_WEATHER_DATA_FILE)


# There is a one hour time shift between the data from the sensors and the 
# Fidas with the winter time change

df_fidas[df_fidas$date<=as.POSIXct("2020-10-25"), ]$date <- df_fidas[df_fidas$date<=as.POSIXct("2020-10-25"), ]$date -as.difftime(1, units = "hours")
df_fidas[df_fidas$date>=as.POSIXct("2021-03-29"), ]$date <- df_fidas[df_fidas$date>=as.POSIXct("2021-03-29"), ]$date -as.difftime(1, units = "hours")

df_join <- df_sensors %>%
  inner_join(dplyr::select(df_fidas, date, PM2.5), by = c("date")) %>%
  inner_join(dplyr::select(df_weather, date, temperature, rh, air_pressure, dew_point_temperature), by=c("date"))


# Remove duplicated rows
df_join <- df_join[!duplicated(df_join), ]

# Cut by month to simplify data visualisation
df_join$month <- cut(df_join$date, breaks = "month")

df_join <- df_join %>%
  mutate(sensor = shorten_sensor_names(sensor, site))


# Remove 2020-10-15 when I did a first experiment with incense sticks.
# Remove 2020-11-03 day of the incense peaks experiment
# and the faulty sensors


df_join <- df_join[(df_join$date>as.POSIXct("2020-10-15 12:00:00") | df_join$date<as.POSIXct("2020-10-15 10:00:00")) &
          (df_join$date>=as.POSIXct("2020-11-03 23:59:59") | df_join$date<=as.POSIXct("2020-11-03 00:00:00")) &
          df_join$sensor != "PMS-60N1" & df_join$sensor != "PMS-56N2" & df_join$sensor != "SPS-40N2", ]


saveRDS(df_join, file = "data/df_join.rds")