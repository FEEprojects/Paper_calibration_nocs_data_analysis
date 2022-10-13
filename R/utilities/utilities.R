require(dplyr)
require(plotly)
require(ggpubr)
require(openair)
require(rstatix)
require(forcats)
require(cowplot)
require(lubridate)
require(splitstackshape)


custom_theme <- theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



#' Daily metrics
#'
#' Provides the daily metrics for the dataset stored in input_folder comparing
#' them with the reference PM2.5 data.frame stored in initial_df_location
#' Exports the aggregated results in output_folder with the scenario_affix name
#'
#' @param input_folder
#' @param output_folder
#' @param scenario_affix
#' @param initial_df_location
#'
#' @return
#' @export
#'
#' @examples
daily_metrics <-
  function(input_folder,
           output_folder,
           scenario_affix,
           initial_df_location = DF_JOINED) {
    df_2min_roll <- readRDS(initial_df_location)
    # get the list of files in the input folder
    file_list <-
      list.files(
        path = input_folder,
        pattern = "df_.*",
        full.names = T,
        recursive = T
        )
    df_2min_roll$cutDate <-
      as.POSIXct(cut(df_2min_roll$date, breaks = "1 day"))
    df_2min_roll_day <- df_2min_roll %>%
      group_by(cutDate, sensor, site) %>%
      summarise(across(where(is.numeric),  ~ mean(.x, na.rm = T))) %>%
      mutate(date = cutDate)
    #initialise dataframes
    df_cal_pms <- data.frame()
    df_train_pms <- data.frame()
    res_cal_pms <- data.frame()
    res_cal_sps <- data.frame()
    
    file <- file_list[1]
    for (file in file_list) {
      name <- strsplit(file, "/")[[1]][9]
      if (grepl("rlmhuber", name)) {
        start_date <- strsplit(strsplit(name, "_")[[1]][6], " ")[[1]][1]
      }
      else{
        start_date <- strsplit(strsplit(name, "_")[[1]][7], " ")[[1]][1]
      }
      if (grepl("initial", name)) {
        next
      }
      
      if (!grepl("train", name)) {
        tmp <- readRDS(file)
        tmp$cutDate <- as.POSIXct(cut(tmp$date, breaks = "1 day"))
        tmp_day <- tmp %>%
          group_by(sensor, cutDate) %>%
          summarise(across(where(is.numeric),  ~ mean(.x, na.rm = T), 
                           .names = "PM25")) %>%
          mutate(date = cutDate)
        tmp_day <- tmp_day %>%
          inner_join(df_2min_roll_day, by = c("sensor", "date"))
        res <-
          calculate_metrics(
            tmp_day,
            sensor_field = "PM25",
            reference_field = "PM2.5",
            method = ""
          )
        res$res_bus$cal_method <- names(tmp)[3]
        res$res_bus$start_date <- start_date
        res$res$cal_method <- names(tmp)[3]
        res$res$start_date <- start_date
        if (grepl("SPS", name)) {
          if (dim(res_cal_sps)[1] == 0) {
            res_bus_cal_sps <- res$res_bus
            res_cal_sps <- res$res
          }
          else{
            res_bus_cal_sps <- bind_rows(res_bus_cal_sps, res$res_bus)
            res_cal_sps <- bind_rows(res_cal_sps, res$res)
          }
        }
        else{
          if (dim(res_cal_pms)[1] == 0) {
            res_bus_cal_pms <- res$res_bus
            res_cal_pms <- res$res
          }
          else{
            res_bus_cal_pms <- bind_rows(res_bus_cal_pms, res$res_bus)
            res_cal_pms <- bind_rows(res_cal_pms, res$res)
          }
        }
      }
    }
    saveRDS(res_cal_sps,
            file = paste0(output_folder, "res_cal_sps_", scenario_affix, ".rds"))
    saveRDS(res_cal_pms,
            file = paste0(output_folder, "res_cal_pms_", scenario_affix, ".rds"))
    saveRDS(
      res_bus_cal_sps,
      file = paste0(output_folder, "res_bus_cal_sps_", scenario_affix, ".rds")
    )
    saveRDS(
      res_bus_cal_pms,
      file = paste0(output_folder, "res_bus_cal_pms_", scenario_affix, ".rds")
    )
}


#' Total least square regression by sensor
#' can be used as a nested operation (see examples)
#' @param df - dataframe containing the time series measurements of a network of PM sensors
#'            with columns:
#'             - sensor_field: PM2.5 readings from the sensor
#'             - reference_field=: PM2.5 readings from the reference instrument used for comparison
#'             - sensor: sensor id
#'             - date: date of the measurement
#' @param sensor_field - name of the columns containing the PM2.5 readings from the sensors
#' @param reference_field - name of the columns containing the PM2.5 readings from the refernce instrument
#' @param ...
#'
#' @return dataframe with slope (b), intercept (a),
#'          uncertainty of slope (ub), of intercept (ua),
#'          whether the slope is significantly equal to 1 (cdt_slope),
#'          whether the intercept is significantly equal to 0 (cdt_intercept)
#'          cm_mean - mean PM2.5 value of the sensor
#'          rm_mean - mean PM2.5 value of the reference instrument
#'          number - number of datapoints used for the regression
#'          Syy - sum of the mean square error of the sensor
#'          Sxx - sum of the mean sqaure error of the reference instrument
#'          Sxy - sum of the cross mean error of the sensor and the reference instrument
#'          r2 - coefficient of determination
#' @export
#'
#' @examples df_join %>%
#'            mutate(yearweek=paste0(year(date)," week number ",week(date))) %>%
#'            group_by(sensor,yearweek) %>%
#'            nest() %>%
#'            dplyr::mutate(regression = map(data, ~possibly(ols,otherwise = tibble(slope = NA, intercept=NA))(.x))) %>%
#'            unnest(regression,.drop=TRUE) ->slope_intercept
ols <-
  function(df,
           sensor_field = "pm25",
           reference_field = "pm2.5",
           ...) {
    OLS_values <- df %>%
      group_by(sensor) %>%
      mutate(cm_mean = mean(.data[[sensor_field]], na.rm = T),
             rm_mean = mean(.data[[reference_field]], na.rm = T)) %>%
      mutate(
        diff_cm_mean = (.data[[sensor_field]] - cm_mean) ^ 2,
        diff_rm_mean = (.data[[reference_field]] - rm_mean) ^ 2,
        cross_diff_mean = (.data[[sensor_field]] - cm_mean) * (.data[[reference_field]] -
                                                                 rm_mean)
      ) %>%
      summarise(
        Syy = sum(diff_cm_mean, na.rm = T),
        Sxx = sum(diff_rm_mean, na.rm = T),
        Sxy = sum(cross_diff_mean, na.rm = T),
        number = n(),
        cm_mean = mean(cm_mean, na.rm = T),
        rm_mean = mean(rm_mean, na.rm = T),
        sum_rm = sum(.data[[reference_field]] ^ 2, na.rm = T)
      )
    
    slopes <- OLS_values %>%
      mutate(
        b = (Syy - Sxx + sqrt((Syy - Sxx) ^ 2 + 4 * Sxy ^ 2)) / (2 * Sxy),
        ub = sqrt((Syy - Sxy ^ 2 / Sxx) / ((number - 2) * Sxx)),
        a = cm_mean - b * rm_mean,
        ua = ub * sqrt(sum_rm / number),
        r2 = (Sxy) ^ 2 / (Sxx * Syy)
      ) %>%
      mutate(
        cdt_slope = ifelse(abs(b - 1) <= 2 * ub, T, F),
        cdt_intercept = ifelse(abs(a) <= 2 * ua, T, F)
      )
    
    return(slopes)
  }



#' Total least square regression based on prcomp function
#' can be used as a nested operation (see examples)
#' @param df - dataframe containing the time series measurements of a network of PM sensors
#'            with columns:
#'             - pm25: PM2.5 readings from the sensor
#'             - pm2.5: PM2.5 readings from the reference instrument used for comparison
#'             - date: date of the measurement
#' @param ...
#'
#' @return tibble with slope and intercept
#' @export
#'
#' @examples df_join %>%
#'            mutate(yearweek=paste0(year(date)," week number ",week(date))) %>%
#'            group_by(sensor,yearweek) %>%
#'            nest() %>%
#'            dplyr::mutate(regression = map(data, ~possibly(TLS,otherwise = tibble(slope = NA, intercept=NA))(.x))) %>%
#'            unnest(regression,.drop=TRUE) ->slope_intercept
TLS <- function(df, ...) {
  if (nrow(df) == 0) {
    warning("dimension of the subset is 0")
    return(tibble(slope = NA, intercept = NA))
  }
  v <- prcomp(na.omit(cbind(df$pm2.5, df$pm25)))$rotation
  slope <- v[2, 1] / v[1, 1]
  intercept <-
    mean(df$pm25, na.rm = TRUE) - mean(df$pm2.5, na.rm = TRUE) * slope
  
  return(tibble(slope = slope, intercept = intercept))
}




#' Expanded uncertainty
#' as defined in the Guide to the demonstration of equivalence of ambient air monitoring methods
#' https://ec.europa.eu/environment/air/quality/legislation/pdf/equivalence.pdf
#'
#' @param df - dataframe containing the time series measurements of a network of PM sensors
#'            with columns:
#'             - sensor: sensor identification number
#'             - pm25: PM2.5 readings from the sensor
#'             - pm2.5: PM2.5 readings from the reference instrument used for comparison
#'             - date: date of the measurement
#' @param slopes - dataframe containing the results of the linear regression between the
#'                 individual sensors and the reference methods with columns:
#'                 - b: slope of the linear regression
#'                 - a: intercept of the linear regression
#' @param limit_value - limit value used for the demonstration of equivalence (30 ug/m3 for PM2.5 in the EU Data Quality Directive)
#'                        however, the limit value for indicative measurement (MCERTS) is 25ug/m3.
#' @param rm_uncertainty - uncertainty of the reference method
#' @param ...
#'
#' @return the expanded uncertainty of the sensors
#' @export
#'
#' @examples expanded_uncertainty(df_joined_small_clean, slopes)
expanded_uncertainty <-
  function(df,
           slopes,
           limit_value = 30,
           rm_uncertainty = 0.61,
           sensor_field = "pm25",
           reference_field = "pm2.5",
           ...) {
    RSS <- inner_join(slopes, df, by = c("sensor")) %>%
      group_by(sensor) %>%
      summarise(RSS = sum((.data[[sensor_field]] - a - b * .data[[reference_field]]) ^
                            2, na.rm = TRUE))
    
    expanded_uncertainty <- inner_join(RSS, slopes, by = c("sensor")) %>%
      group_by(sensor) %>%
      mutate(
        random_term = ifelse(
          RSS / (number - 2) - rm_uncertainty ^ 2 < 0,
          0,
          sqrt(RSS / (number - 2) - rm_uncertainty ^ 2)
        ),
        bias_lv = (a + (b - 1) * limit_value)
      ) %>%
      mutate(
        combined_uncertainty = sqrt(random_term ^ 2 + bias_lv ^ 2),
        expanded_uncertainty = 2 * combined_uncertainty / (limit_value)
      )
    expanded_uncertainty
  }



#' Between sampler uncertainty
#'
#' Compute the uncertainty between sensors/samplers according to the EU Guidance for equivalence
#'
#' @param df : data.frame containing the following columns:
#'               - sensor: id of the sensors to compare
#'               - sensor_field: field containing the PM2.5 readings of the sensors
#' @param sensor_field : name of the column containing the PM2.5 readings of the sensors
#' @param ...
#'
#' @return data.frame containing:
#'          - usb: between sampler uncertainty (ug/m3)
#'          - combination: combination of the sensors used
#'          - nbr: nombre of data points per combination
#' @export
#'
#' @examples
between_sampler_uncertainty <-
  function(df, sensor_field = "median_PM25", ...) {
    df <- df <- df[!is.na(df$sensor), ] %>%
      group_by(site, sensor) %>%
      arrange(site, sensor)
    
    sensor_combinations <- combn(paste0(unique(df$sensor)), 2)
    res <- tibble()
    for (i in 1:ncol(sensor_combinations)) {
      sensor_1 <- as.character(sensor_combinations[1, i])
      sensor_2 <- as.character(sensor_combinations[2, i])
      combination <- paste0(sensor_1, " - ", sensor_2)
      
      if (grepl("PMS", combination) & grepl("SPS", combination)) {
        next
      }
      
      tmp <- df[df$sensor %in% c(sensor_1, sensor_2),] %>%
        ungroup() %>%
        dplyr::select(date, sensor, .data[[sensor_field]]) %>%
        distinct() %>%
        pivot_wider(
          values_from = .data[[sensor_field]],
          names_from = sensor,
          values_fn = function(x) {
            mean(x, na.rm = T)
          }
        ) %>%
        dplyr::select(sensor_1, sensor_2, date) %>%
        unnest(cols = everything()) %>%
        summarise(ubs2 = sum((.data[[sensor_1]] - .data[[sensor_2]]) ^ 2, na.rm =
                               T), nbr = n()) %>%
        mutate(ubs2 = ubs2 / (2 * nbr)) %>%
        mutate(usb = sqrt(ubs2)) %>%
        dplyr::select(-ubs2) %>%
        mutate(combination = paste0(sensor_1, " - ", sensor_2))
      res <- bind_rows(res, tmp)
      rm(tmp)
    }
    res
}


#' Wrapper for between_sampler_uncertainty
#'
#' The EU guidance require to calculate the between sampler uncertainty for three datasets:
#'  - full dataset
#'  - concentrations below 18ug/m3
#'  - concentrations higher than 18ug/m3
#'
#' @param df
#' @param sensor_field
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
between_sampler_uncertainty_split_datasets <-
  function(df,
           sensor_field = "median_PM25",
           reference_field = "PM2.5",
           ...) {
    df_lt18 <- df[df[[reference_field]] < 18,]
    df_gt18 <- df[df[[reference_field]] >= 18, ]
    
    res_full <-
      between_sampler_uncertainty(df, sensor_field = sensor_field)
    res_full$type <- "Full"
    
    if (!dim(df_lt18)[1] == 0) {
      res_lt18 <-
        between_sampler_uncertainty(df_lt18, sensor_field = sensor_field)
      res_lt18$type <- "<18"
    }
    else{
      res_lt18 <- res_full[c(), ]
      res_lt18$type <- "<18"
    }
    if (!dim(df_gt18)[1] == 0) {
      res_gt18 <-
        between_sampler_uncertainty(df_gt18, sensor_field = sensor_field)
      res_gt18$type <- ">18"
    }
    else{
      res_gt18 <- res_full[c(), ]
      res_gt18$type <- ">18"
    }
    bind_rows(res_full, res_lt18, res_gt18)
  }


#' Expanded uncertainty between split datasets
#' 
#' Splits the data into three subsets:
#' 1. the full dataset
#' 2. dataset when PM2.5<18ug/m3
#' 3. dataset when PM2.5>=18ug/m3
#'
#' @param df 
#' @param slopes 
#' @param limit_value 
#' @param rm_uncertainty 
#' @param sensor_field 
#' @param reference_field 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
expanded_uncertainty_split_datasets <-
  function(df,
           slopes,
           limit_value = 30,
           rm_uncertainty = 0.61,
           sensor_field = "pm25",
           reference_field = "pm2.5",
           ...) {
    df_lt18 <- df[df[[reference_field]] < 18,]
    df_gt18 <- df[df[[reference_field]] >= 18, ]
    
    res_full <- expanded_uncertainty(
      df,
      slopes = slopes,
      limit_value = limit_value,
      rm_uncertainty = rm_uncertainty,
      sensor_field = sensor_field,
      reference_field = reference_field
    )
    res_full$type <- "Full"
    if (!dim(df_lt18)[1] == 0) {
      res_lt18 <- expanded_uncertainty(
        df_lt18,
        slopes = slopes,
        limit_value = limit_value,
        rm_uncertainty = rm_uncertainty,
        sensor_field = sensor_field,
        reference_field = reference_field
      )
    }
    else{
      res_lt18 <- res_full[c(), ]
    }
    res_lt18$type <- "<18"
    if (!dim(df_gt18)[1] == 0) {
      res_gt18 <- expanded_uncertainty(
        df_gt18,
        slopes = slopes,
        limit_value = limit_value,
        rm_uncertainty = rm_uncertainty,
        sensor_field = sensor_field,
        reference_field = reference_field
      )
    }
    else{
      res_gt18 <- res_full[c(), ]
    }
    res_gt18$type <- ">18"
    
    bind_rows(res_full, res_lt18, res_gt18)
    
  }



#' Coefficient of variation
#' as defined in Appendix B, part 3 of
#' National Institute of Occupational Safety and Health. Components for Evaluation of Direct-Reading Monitors for Gases and Vapors. (2012).
#'
#'This calculates the CV for each sensor individually
#'
#' @param df - dataframe containing the time series measurements of a network of PM sensors
#'            with columns:
#'             - sensor: sensor identification number
#'             - sensor_type: model of sensor to be analysed
#'             - pm25: PM2.5 readings from the sensor
#'             - date: date of the measurement
#' @param ...
#'
#' @return CV - coefficient of variation
#'         k - number of concentrations levels of the reference instrument
#'         n - average number of datapoints per concentration levels
#'         y__ - bias
#'         U - expanded uncertainty (according to NIOHS)
#' @export
#'
#' @examples coefficient_of_variation(df)
coefficient_of_variation <-
  function(df,
           sensor_field = "pm25",
           reference_field = "PM2.5",
           rounding = 1,
           ...) {
    df$PM2.5_round <- round2(df[[reference_field]], n = rounding)
    
    cv <- df %>%
      #get_sensor_type() %>%
      #filter(sensor != "SHT35") %>%
      group_by(PM2.5_round, sensor) %>% # relative standard deviation per concentration level
      summarise(
        sd = sd(.data[[sensor_field]], na.rm = TRUE),
        count = n(),
        mean = mean(.data[[reference_field]], na.rm = TRUE)
      ) %>%
      #filter(sd!=0) %>% #Work (checked)
      mutate(CV = sd / mean) %>% # Calculate the CV per sensor at each concentration level
      mutate(sum_cv_squared = CV * CV * (count - 1)) %>%
      ungroup() %>%
      group_by(sensor) %>% # Calculate the CV for each sensor_type
      summarise(cv = sqrt(sum(sum_cv_squared / sum(count - 1), na.rm = TRUE)),
                k = n(),
                n = mean(count))
    
    df$y <-
      df[[sensor_field]] / df[[reference_field]] - 1 #bias of each datapoint
    yi_ <- df %>% #bias per concentration level
      group_by(PM2.5_round, sensor) %>%
      summarise(nb = n(), yi_ = sum(y, na.rm = TRUE) / nb)
    
    y__ <- yi_ %>% #mean bias
      ungroup() %>%
      group_by(sensor) %>%
      summarise(k = n(), y__ = sum(yi_, na.rm = T) / k) %>%
      unique()
    
    cv %>%
      inner_join(dplyr::select(y__, -k), by = c("sensor")) %>%
      mutate(U = 1.96 * sqrt(y__ ^ 2 + cv ^ 2))
  }



#' Coefficient of variation
#' as defined in Appendix C of
#' National Institute of Occupational Safety and Health. Components for Evaluation of Direct-Reading Monitors for Gases and Vapors. (2012).
#'
#' @param df - dataframe containing the time series measurements of a network of PM sensors
#'            with columns:
#'             - sensor: sensor identification number
#'             - sensor_type: model of sensor to be analysed
#'             - sensor_field: PM2.5 readings from the sensor
#'             - date: date of the measurement
#' @param sensor_field - name of the field containing the PM2.5 from the sensor
#'
#' @param reference_field - name of the field containing the PM2.5 from the reference instrument
#'
#'
#' @return tibble - with columns:
#'                  - sensor_type:
#'                  - msb: or intramodel bias
#'                  - u: number of sensors of the types included
#'                  - mse:
#'                  - k: number of concentration levels or "trials"
#'                  - cv: intramodel precision or coefficient of variation
#'                  - y__: bias if the bias is homogeneous
#'                  - U: expanded uncertainty intramodel (according to NIOHS)
#'
#' @export
#'
#' @examples coefficient_of_variation_multiple(df)
#df$PM2.5_round <- round2(df[[reference_field]],n = rounding)
coefficient_of_variation_multiple <-
  function(df,
           sensor_field = "pm25",
           reference_field = "PM2.5",
           ...) {
    df$y <- df[[sensor_field]] / df[[reference_field]] - 1
    yi_ <- df %>%
      get_sensor_type() %>%
      group_by(sensor_type, sensor) %>%
      summarise(k = n(), yi_ = sum(y, na.rm = TRUE) / k)
    
    y_l <- df %>%
      get_sensor_type() %>%
      group_by(sensor_type, date) %>%
      summarise(u = n(), y_l = sum(y, na.rm = TRUE) / u)
    
    u <- y_l %>%
      group_by(sensor_type) %>%
      summarise(u = mean(u, na.rm = T))
    # y__ is the bias if the bias is homogeneous
    y__ <- yi_ %>%
      inner_join(u, by = c("sensor_type")) %>%
      ungroup() %>%
      group_by(sensor_type) %>%
      summarise(y__ = sum(yi_, na.rm = T) / u, u = mean(u, na.rm = T)) %>%
      unique()
    
    
    
    mse <- df %>%
      get_sensor_type() %>%
      inner_join(yi_, by = c("sensor", "sensor_type")) %>%
      inner_join(y_l, by = c("sensor_type", "date")) %>%
      inner_join(dplyr::select(y__, -u), by = c("sensor_type")) %>%
      group_by(sensor_type) %>%
      summarise(
        mse = 1 / (mean(k, na.rm = TRUE) - 1) / (mean(u, na.rm = TRUE) - 1) * sum((y -
                                                                                     yi_ - y_l + y__) ^ 2, na.rm = TRUE),
        k = mean(k, na.rm = TRUE)
      )
    
    mse
    
    msb <- yi_ %>%
      inner_join(y__, by = c("sensor_type")) %>%
      group_by(sensor_type) %>%
      summarise(msb = 1 / (u - 1) * sum(k * (yi_ - y__) ^ 2, na.rm = T),
                u = mean(u, na.rm = T)) %>%
      unique()
    msb
    
    msb %>%
      inner_join(mse) %>%
      mutate(cv = sqrt((msb - mse) / k)) %>%
      inner_join(y__, by = c("sensor_type", "u")) %>%
      mutate(U = 1.96 * sqrt(y__ ^ 2 + cv ^ 2)) #Expanded uncertainty
    
    
  }



#' Extract the sensor type from the sensor id
#' sensor id must respect the format sensorType_idNumber
#'
#' @param df - dataframe containing the time series measurements of a network of PM sensors
#'            with columns:
#'             - sensor: sensor identification number - sensorType_idNumber
#'
#' @param ...
#'
#' @return dataframe with column sensor_type added
#'
#' @export
#'
#' @examples get_sensor_type(df)
get_sensor_type <- function(df, ...) {
  
  cSplit(
    indt = df,
    splitCols = c("sensor"),
    sep = "-",
    direction = "wide",
    drop = FALSE
  ) %>%
    dplyr::select(-sensor_2) %>%
    rename(sensor_type = sensor_1)
  
}

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x) * 10 ^ n
  z = z + 0.5
  z = trunc(z)
  z = z / 10 ^ n
  z * posneg
}



#' shapeDTW
#'
#' This function takes the first order difference of the signals contained
#' in @query and uses the package "dtw", with the step pattern RabinerJuang 4a,
#' to align the time series from the reference (PM2.5) and the sensors (roll_pm25)
#'
#' It returns a list containing (1) the query containing the reading of the sensors aligned with the reference
#' instrument and (2) the distance and normalised distance of the alignement
#'
#' @param query - data.frame ordered by date containing at least two columns: roll_pm25 which contains the
#' readings from the sensors and PM2.5 which contain the readings from the reference instrument
#'
#' @return list(data = df, metrics = df)
#' @export
#'
#' @examples
shapeDTW <-
  function(query,
           ref = "PM2.5",
           sensor = "roll_pm25",
           pattern = rabinerJuangStepPattern(3, "c")) {
    query <- query[!is.na(query[[sensor]]) & !is.na(query[[ref]]),]
    if (length(query) == 0) {
      return(list(data = NA, metrics = NA))
    }
    query_diff <-
      finite.differences(as.numeric(query$date), query[[sensor]])
    template_diff <-
      finite.differences(as.numeric(query$date), query[[ref]])
    differences <-
      bind_cols(query = query_diff, template = template_diff)
    alignment <- dtw(query_diff, template_diff, step.pattern = pattern)
    wq <- warp(alignment, index.reference = FALSE)
    query_1 <- query[round2(wq, n = 0), ]
    query_1$date <- query$date
    # Check that the correction is better than the original
    query_corr <- cor(query[[sensor]], query[[ref]])
    query_1_corr <- cor(query_1[[sensor]], query_1[[ref]])
    if (query_corr >= query_1_corr) {
      message("No correction")
      query_1 <- query
      alignment$distance <- NA
      alignment$normalizedDistance <- NA
    }
    
    return(list(
      data = query_1,
      metrics = data.frame(
        distance = alignment$distance,
        normalizedDistance = alignment$normalizedDistance
      )
    ))
    
  }




#' RMSE
#' 
#' Compute the RMSE between the columns sensor_field and reference_field of
#' the dataframe df
#'
#' @param df - dataframe containing data from multiple sensors
#' @param sensor_field - column name containing the data from the sensors
#' @param reference_field - column name containing the data from the reference
#'  instrument
#' @param ... 
#'
#' @return dataframe containing the RMSE per sensor
#' @export
#'
#' @examples
rmse <- function(df, sensor_field, reference_field, ...) {
  df %>%
    group_by(sensor) %>%
    mutate(error = .data[[sensor_field]] - .data[[reference_field]]) %>%
    summarise(RMSE = sqrt(sum(error ^ 2, na.rm = T) / n()))
}


#' Calculate expanded uncertainty and CV
#'
#' This function calculates the expanded uncertainty, the coefficient of variation,
#' the RMSE and the bias of a dataframe df_pm_corr grouped by sensor
#'
#' @param df_pm_corr
#'        Contains a dataframe that has been corrected (or not) by a calibration method
#'        which results have been stored in res_model
#' @param df_pm_corr_test
#'        Test data set sampled from df_pm_corr
#' @param res_model
#'        Results of the calibration model (only used to get the AIC)
#' @param sensor_field
#' @param reference_field
#' @param name
#'        Name of the calibration method tested
#'
#' @return
#' @export
#'
#' @examples
calculate_expanded_uncertainty_cv <-
  function(df_pm_corr,
           df_pm_corr_test,
           res_model,
           sensor_field = "PM25_corr",
           reference_field = "roll_Fidas_pm25_2min",
           name) {
    # Expanded uncertainty - test
    if (!is.na(df_pm_corr_test)) {
      res_ols_model_test <-
        ols(df_pm_corr_test,
            sensor_field = sensor_field,
            reference_field = reference_field)
      res_expanded_uncertainty_model_test <-
        expanded_uncertainty(
          df_pm_corr_test,
          slopes = res_ols_model_test,
          sensor_field = sensor_field,
          reference_field = reference_field
        )
      res_expanded_uncertainty_model_test$method = paste0(name, " - Test")
      res_expanded_uncertainty_model_test$RMSE <-
        rmse(df_pm_corr_test,
             sensor_field = sensor_field,
             reference_field = reference_field)$RMSE
      bias <-
        bias(df_pm_corr_test,
             sensor_field = sensor_field,
             reference_field = reference_field)
      res_expanded_uncertainty_model_test <-
        inner_join(
          res_expanded_uncertainty_model_test,
          dplyr::select(bias, sensor, bias),
          by = c("sensor")
        )
      #inner_join(res_expanded_uncertainty_test, by=c("sensor"))
      if (is.na(res_model)) {
        res_expanded_uncertainty_model_test$AIC <- NA
      }
      else{
        res_expanded_uncertainty_model_test$AIC <- res_model$AIC
      }
      #res_expanded_uncertainty_test <<- bind_rows(res_expanded_uncertainty_test, res_expanded_uncertainty_model_test)
      # Coefficient of variation - test
      res_cv_model_test <-
        df_pm_corr_test %>% coefficient_of_variation(sensor = sensor_field)
      res_cv_model_test$method <- paste0(name, " - Test")
      #res_cv_test<<-bind_rows(res_cv_test, res_cv_model_test)
    }
    else{
      res_ols_model_test <- NA
      res_cv_model_test <- NA
      res_expanded_uncertainty_model_test <- NA
    }
    
    # Expanded uncertainty - full
    res_ols_model <-
      ols(df_pm_corr,
          sensor_field = sensor_field,
          reference_field = reference_field)
    res_expanded_uncertainty_model <-
      expanded_uncertainty(
        df_pm_corr,
        slopes = res_ols_model,
        sensor_field = sensor_field,
        reference_field = reference_field
      )
    res_expanded_uncertainty_model$method = paste0(name, " - Full")
    res_expanded_uncertainty_model$RMSE <-
      rmse(df_pm_corr,
           sensor_field = sensor_field,
           reference_field = reference_field)$RMSE
    bias <-
      bias(df_pm_corr,
           sensor_field = sensor_field,
           reference_field = reference_field)
    res_expanded_uncertainty_model <-
      inner_join(res_expanded_uncertainty_model,
                 dplyr::select(bias, sensor, bias),
                 by = c("sensor"))
    #inner_join(res_expanded_uncertainty, by=c("sensor"))
    
    if (is.na(res_model)) {
      res_expanded_uncertainty_model$AIC <- NA
    }
    else{
      res_expanded_uncertainty_model$AIC <- res_model$AIC
    }
    #res_expanded_uncertainty <<- bind_rows(res_expanded_uncertainty, res_expanded_uncertainty_model)
    # Coefficient of variation - full
    res_cv_model <-
      df_pm_corr %>% coefficient_of_variation(sensor = sensor_field)
    
    res_cv_model$method <- paste0(name, " - Full")
    return(
      list(
        "res_ols_model_test" = res_ols_model_test,
        "res_ols_model" = res_ols_model,
        "res_cv_model" = res_cv_model,
        "res_cv_model_test" = res_cv_model_test,
        "res_expanded_uncertainty_model" = res_expanded_uncertainty_model,
        "res_expanded_uncertainty_model_test" = res_expanded_uncertainty_model_test
      )
    )
  }

#' Homogeneity of the bias
#'
#' This function computes the F statistic and its p-value according to page 51 of NIOHS2012
#'
#' @param df
#' @param sensor_field
#' @param reference_field
#' @param nbre
#' @param rounding
#'
#' @return
#' @export
#'
#' @examples
homogeneity_bias <- function(df,
                             sensor_field = "median_PM25",
                             reference_field = "PM2.5",
                             nbre,
                             rounding = 1) {
  # Transform the variable x to y.
  df$y <- df[[sensor_field]] / df[[reference_field]] - 1
  
  df$PM2.5_round <- round2(df[[reference_field]], n = rounding)
  
  df_sampled <- df %>%
    group_by(sensor, PM2.5_round) %>%
    mutate(nb = n()) %>%
    filter(nb >= nbre) %>%
    slice_sample(n = nbre)
  
  yi_ <- df_sampled %>%
    group_by(sensor, PM2.5_round) %>%
    summarise(nb = n(), yi_ = sum(y, na.rm = T)) %>%
    mutate(yi_ = yi_ / nb)
  
  y__ <- yi_ %>%
    group_by(sensor) %>%
    summarise(y__ = mean(yi_, na.rm = T), k = n())
  
  ssb <- yi_ %>%
    inner_join(y__, by = c("sensor")) %>%
    group_by(sensor, k) %>%
    summarise(ssb = sum(nb * (yi_ - y__) ^ 2, na.rm = T))
  
  ssw <- df %>%
    inner_join(yi_, by = c("sensor", "PM2.5_round")) %>%
    group_by(sensor, PM2.5_round) %>%
    summarise(sum_1 = sum((y - yi_) ^ 2, na.rm = T)) %>%
    ungroup() %>%
    group_by(sensor) %>%
    summarise(ssw = sum(sum_1, na.rm = T))
  
  ssw %>%
    inner_join(ssb, by = c("sensor")) %>%
    mutate(F_ = ssb / (k - 1) / (ssw / (k * (nbre - 1)))) %>%
    mutate(p_value = pf(F_, k - 1, k * (nbre - 1)))
  
}




#' Homogeneity of the precision.
#'
#' This function computes the H statistic and its p-value according to page 56 of NIOHS2012
#'
#' @param df
#' @param sensor_field
#' @param reference_field
#' @param nbre
#' @param rounding
#'
#' @return
#' @export
#'
#' @examples
homogeneity_precision <- function(df,
                                  sensor_field = "median_PM25",
                                  reference_field = "PM2.5",
                                  nbre,
                                  rounding = 1) {
  df$y <- df[[sensor_field]] / df[[reference_field]] - 1
  nbre <- 50
  df$PM2.5_round <- round2(df[[reference_field]], n = rounding)
  
  df_sampled <- df %>%
    group_by(sensor, PM2.5_round) %>%
    mutate(nb = n()) %>%
    filter(nb >= nbre) %>%
    slice_sample(n = nbre)
  
  yi_ <- df_sampled %>%
    group_by(sensor, PM2.5_round) %>%
    summarise(nb = n(), yi_ = sum(y, na.rm = T)) %>%
    mutate(yi_ = yi_ / nb)
  
  sigma_i_2 <- df_sampled %>%
    inner_join(yi_, by = c("sensor", "PM2.5_round")) %>%
    group_by(sensor, PM2.5_round) %>%
    summarise(sigma_i_2 = sum((y - yi_) ^ 2, na.rm = T), nb = n()) %>%
    mutate(sigma_i_2 = sigma_i_2 / nb)
  
  sigma_e_2 <- sigma_i_2 %>%
    group_by(sensor) %>%
    summarise(sigma_e_2 = sum(sigma_i_2, na.rm = T), k = n()) %>%
    mutate(sigma_e_2 = sigma_e_2 / k)
  sigma_e_2
  sigma_i_2
  H <- sigma_i_2 %>%
    inner_join(sigma_e_2, by = c("sensor")) %>%
    ungroup() %>%
    group_by(sensor) %>%
    summarise(H = (k * (nbre - 1) * log10(sigma_e_2) - (nbre - 1) * sum(log10(sigma_i_2), na.rm =
                                                                          T)) / (1 + (k + 1) / (3 * k * (nbre - 1))),
              k = mean(k, na.rm = T)) %>%
    mutate(p_value = pchisq(H, k - 1))
  unique(H)
}


#' Wrapper for all the metrics used
calculate_metrics <- function(df,
                              sensor_field = "median_PM25",
                              reference_field = "PM2.5",
                              limit_value = 30,
                              rm_uncertainty = 0.61,
                              method,
                              bus = TRUE,
                              ...) {
  if (bus == TRUE) {
    res_bus <-
      between_sampler_uncertainty_split_datasets(df, sensor_field = sensor_field,
                                                 reference_field = reference_field)
    res_bus$method <- method
  }
  
  RMSE <- rmse(df, sensor_field = sensor_field,
               reference_field = reference_field)
  cv <- coefficient_of_variation(df, sensor_field = sensor_field,
                                 reference_field = reference_field)
  
  eu <-
    expanded_uncertainty_split_datasets(
      df,
      ols(df, sensor_field = sensor_field,
          reference_field = reference_field),
      sensor_field = sensor_field,
      reference_field = reference_field,
      limit_value = limit_value,
      rm_uncertainty = rm_uncertainty
    ) %>%
    dplyr::select(sensor, b, a, r2, expanded_uncertainty, ua, ub, type)
  
  res <- RMSE %>%
    inner_join(cv, by = c("sensor")) %>%
    inner_join(eu, by = c("sensor"))
  
  
  res$method <- method
  if (bus == TRUE) {
    list(res = res, res_bus = res_bus)
  }
  else{
    list(res = res)
  }
  
}

remove_outliers <- function(df, sensor_field = "pm25", ...) {
  #' To be used within a nested data.frame per date and per sensor type.
  #'Based on the deviation to 3 times the median absolute deviation
  pm25_median  = median(df[[sensor_field]], na.rm = TRUE)
  pm25_mad = mad(df[[sensor_field]], na.rm = TRUE)
  
  df <- df %>%
    mutate(no_outliers = ifelse(abs(.data[[sensor_field]] - pm25_median) >=
                                  3 * pm25_mad, NA, .data[[sensor_field]]))
  return(df)
}

shorten_sensor_names <- function(sensor_name, site) {
  #' Aggregetate the sensor and site fields
  paste0(
    substr(sensor_name, 1, 3),
    "-",
    substr(sensor_name, nchar(as.character(sensor_name)) - 1,
           nchar(as.character(sensor_name))),
    "N",
    substr(site, 6, 6)
  )
}
