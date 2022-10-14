require(gbm)
require(e1071)
require(MASS)
require(dplyr)

#' Clean regression
#'
#' Linear regression values with y = reference, x = sensor
#'
#' @param df - dataframe containing the time series measurements of a network of PM sensors
#'            with columns:
#'             - sensor: sensor identification number
#'             - pm25: PM2.5 readings from the sensor
#'             - pm2.5: PM2.5 readings of the reference instrument
#'             - date: date of the measurement
#' @param threshold - exclude sensor readings that are below a threshold (useful to account for the limit of detection of the sensors)
#' @param y - column used as y for lm
#' @param x - column used as x for lm
#' @param ... 
#'
#' @return tibble
#'
#' @export
#'
#' @examples clean_regression(df)
#'           df %>%
#'           group_by(sensor) %>%
#'           nest() %>%
#'           mutate(regression = map(data, ~clean_regression(df =.x))) %>%
#'           unnest(regression, .drop=TRUE)
clean_regression<-function(df,  y = "pm2.5", x = "pm25", ...){
  
  

  
  res<-lm(df[[y]]~df[[x]]) 
  
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  
  spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope = coeffs[2,]$estimate, slope_std =coeffs[2,]$std.error, slope_p_value =coeffs[2,]$p.value,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_p_value =coeffs[1,]$p.value,
                r_squared_adjusted = stats[1,]$adj.r.squared, r_squared = stats[1,]$r.squared, 
                r_squared_p_value =stats[1,]$p.value,  spearman = spearman))
  
}

#' Multi-linear regression with 1 parameter
#'
#' Linear regression values with y = reference, x = sensor
#'
#' @param df - dataframe containing the time series measurements of a network of PM sensors
#'            with columns:
#'             - sensor: sensor identification number
#'             - pm25: PM2.5 readings from the sensor
#'             - pm2.5: PM2.5 readings of the reference instrument
#'             - date: date of the measurement
#'             - rh: relative humidity
#' @param threshold - exclude sensor readings that are below a threshold (useful to account for the limit of detection of the sensors)
#' @param y - column used as y for lm
#' @param x - column used as x for lm
#' @param rh - column used for relative humidity
#' @param ... 
#'
#' @return tibble
#'
#' @export
#'
#' @examples mlr_1(df)
#'           df %>%
#'           group_by(sensor) %>%
#'           nest() %>%
#'           mutate(regression = map(data, ~mlr_1(df =.x))) %>%
#'           unnest(regression, .drop=TRUE)
mlr_1<-function(df, threshold =0, y = "pm2.5", x = "pm25", rh="rh", ...){
  
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  
  res<-lm(df[[y]]~df[[x]]+df[[rh]]) 
  
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_x = coeffs[2,]$estimate, slope_x_std =coeffs[2,]$std.error, slope_x_p_value =coeffs[2,]$p.value,
                slope_rh = coeffs[3,]$estimate, slope_rh_std =coeffs[3,]$std.error, slope_rh_p_value =coeffs[3,]$p.value,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_p_value =coeffs[1,]$p.value,
                r_squared_adjusted = stats[1,]$adj.r.squared, r_squared = stats[1,]$r.squared, 
                r_squared_p_value =stats[1,]$p.value,
                AIC = stats[1,]$AIC))
  
}

#' Multi-linear regression with multiple parameters
#'
#' Linear regression values with y = reference, x = sensor
#'
#' @param df - dataframe containing the time series measurements of a network of PM sensors
#'            with columns:
#'             - sensor: sensor identification number
#'             - pm25: PM2.5 readings from the sensor
#'             - pm2.5: PM2.5 readings of the reference instrument
#'             - date: date of the measurement
#'             - rh: relative humidity
#' @param threshold - exclude sensor readings that are below a threshold (useful to account for the limit of detection of the sensors)
#' @param y - column used as y for lm
#' @param x - column used as x for lm
#' @param rh - column used for relative humidity
#' @param ... 
#'
#' @return tibble
#'
#' @export
#'
#' @examples mlr_1(df)
#'           df %>%
#'           group_by(sensor) %>%
#'           nest() %>%
#'           mutate(regression = map(data, ~mlr_1(df =.x))) %>%
#'           unnest(regression, .drop=TRUE)
mlr_2<-function(df, threshold =0, y = "pm2.5", x = "pm25", rh="rh", temperature="temperature",...){
  
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  
  res<-lm(df[[y]]~df[[x]]+df[[rh]]+df[[temperature]]) 
  
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_x = coeffs[2,]$estimate, slope_x_std =coeffs[2,]$std.error, slope_x_p_value =coeffs[2,]$p.value,
                slope_rh = coeffs[3,]$estimate, slope_rh_std =coeffs[3,]$std.error, slope_rh_p_value =coeffs[3,]$p.value,
                slope_temp = coeffs[4,]$estimate, slope_temp_std =coeffs[4,]$std.error, slope_temp_p_value =coeffs[4,]$p.value,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_p_value =coeffs[1,]$p.value,
                r_squared_adjusted = stats[1,]$adj.r.squared, r_squared = stats[1,]$r.squared, 
                r_squared_p_value =stats[1,]$p.value,
                AIC = stats[1,]$AIC))
  
}


#' RLM Nest
#' 
#' Robust linear model nested
#'
#' @param df 
#' @param threshold 
#' @param y 
#' @param x 
#' @param psi_function 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
rlm_nest <- function(df, threshold = 0, y = "pm2.5", x = "pm25",  psi_function = psi.huber, ...){
  
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  
  res<-rlm(df[[y]]~df[[x]], psi=psi_function) 
  
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_x = coeffs[2,]$estimate, slope_x_std =coeffs[2,]$std.error, slope_x_t_value =coeffs[2,]$statistic,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_t_value =coeffs[1,]$statistic,
                sigma = stats[1,]$sigma,
                AIC = stats[1,]$AIC))
  
}

rlm_rh_nest<-function(df, threshold =0, y = "pm2.5", x = "pm25",  rh="rh", psi_function=psi.huber, ...){
  
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  
  res<-rlm(df[[y]]~df[[x]]+df[[rh]], psi=psi_function) 
  
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_x = coeffs[2,]$estimate, slope_x_std =coeffs[2,]$std.error, slope_x_t_value =coeffs[2,]$statistic,
                slope_rh = coeffs[3,]$estimate, slope_rh_std =coeffs[3,]$std.error, slope_rh_t_value =coeffs[3,]$statistic,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_t_value =coeffs[1,]$statistic,
                sigma = stats[1,]$sigma,
                AIC = stats[1,]$AIC))
  
}

rlm_part_rh_sps_nest<-function(df, threshold =0, y = "pm2.5",  rh="rh", bin_field, psi_function=psi.huber, ...){
  
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  
  res<-rlm(df[[y]] ~ df[[rh]] + df[[bin_field[1]]] + df[[bin_field[2]]], psi=psi_function) 
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #corr <- res %>% broom::augment()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_rh = coeffs[2,]$estimate, slope_rh_std =coeffs[2,]$std.error, slope_rh_t_value =coeffs[2,]$statistic,
                slope_n05 = coeffs[3,]$estimate, slope_n05_std =coeffs[3,]$std.error, slope_n05_t_value =coeffs[3,]$statistic,
                slope_n1 = coeffs[4,]$estimate, slope_n1_std =coeffs[4,]$std.error, slope_n1_t_value =coeffs[4,]$statistic,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_t_value =coeffs[1,]$statistic,
                sigma = stats[1,]$sigma,
                AIC = stats[1,]$AIC))
  
}


rlm_part_rh_pms_nest<-function(df, threshold =0, y = "pm2.5",  rh="rh", bin_field,  psi_function=psi.huber, ...){
  
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  
  res<-rlm(df[[y]] ~ df[[rh]] + df[[bin_field[1]]] + df[[bin_field[2]]] + df[[bin_field[3]]] + df[[bin_field[4]]] + df[[bin_field[5]]] + df[[bin_field[6]]], psi=psi_function) 
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #corr <- res %>% broom::augment()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_rh = coeffs[2,]$estimate, slope_rh_std =coeffs[2,]$std.error, slope_rh_t_value =coeffs[2,]$statistic,
                slope_gr03um = coeffs[3,]$estimate, slope_gr03um_std =coeffs[3,]$std.error, slope_gr03um_t_value =coeffs[3,]$statistic,
                slope_gr05um = coeffs[4,]$estimate, slope_gr05um_std =coeffs[4,]$std.error, slope_gr05um_t_value =coeffs[4,]$statistic,
                slope_gr10um = coeffs[5,]$estimate, slope_gr10um_std =coeffs[5,]$std.error, slope_gr10um_t_value =coeffs[5,]$statistic,
                slope_gr25um = coeffs[6,]$estimate, slope_gr25um_std =coeffs[6,]$std.error, slope_gr25um_t_value =coeffs[6,]$statistic,
                slope_gr50um = coeffs[7,]$estimate, slope_gr50um_std =coeffs[7,]$std.error, slope_gr50um_t_value =coeffs[7,]$statistic,
                slope_gr100um = coeffs[8,]$estimate, slope_gr100um_std =coeffs[8,]$std.error, slope_gr100um_t_value =coeffs[8,]$statistic,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_t_value =coeffs[1,]$statistic,
                sigma = stats[1,]$sigma,
                AIC = stats[1,]$AIC))
  
}


rlm_part_sps_nest<-function(df, threshold =0, y = "pm2.5", bin_field, psi_function=psi.huber, ...){
  
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  
  res<-rlm(df[[y]] ~  df[[bin_field[1]]] + df[[bin_field[2]]], psi=psi_function) 
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #corr <- res %>% broom::augment()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_n05 = coeffs[2,]$estimate, slope_n05_std =coeffs[2,]$std.error, slope_n05_t_value =coeffs[2,]$statistic,
                slope_n1 = coeffs[3,]$estimate, slope_n1_std =coeffs[3,]$std.error, slope_n1_t_value =coeffs[3,]$statistic,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_t_value =coeffs[1,]$statistic,
                sigma = stats[1,]$sigma,
                AIC = stats[1,]$AIC))
  
}


rlm_part_pms_nest<-function(df, threshold =0, y = "pm2.5",   bin_field,  psi_function=psi.huber, ...){
  
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  
  res<-rlm(df[[y]] ~  df[[bin_field[1]]] + df[[bin_field[2]]] + df[[bin_field[3]]] + df[[bin_field[4]]] + df[[bin_field[5]]] + df[[bin_field[6]]], psi=psi_function) 
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #corr <- res %>% broom::augment()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_gr03um = coeffs[2,]$estimate, slope_gr03um_std =coeffs[2,]$std.error, slope_gr03um_t_value =coeffs[2,]$statistic,
                slope_gr05um = coeffs[3,]$estimate, slope_gr05um_std =coeffs[3,]$std.error, slope_gr05um_t_value =coeffs[3,]$statistic,
                slope_gr10um = coeffs[4,]$estimate, slope_gr10um_std =coeffs[4,]$std.error, slope_gr10um_t_value =coeffs[4,]$statistic,
                slope_gr25um = coeffs[5,]$estimate, slope_gr25um_std =coeffs[5,]$std.error, slope_gr25um_t_value =coeffs[5,]$statistic,
                slope_gr50um = coeffs[6,]$estimate, slope_gr50um_std =coeffs[6,]$std.error, slope_gr50um_t_value =coeffs[6,]$statistic,
                slope_gr100um = coeffs[7,]$estimate, slope_gr100um_std =coeffs[7,]$std.error, slope_gr100um_t_value =coeffs[7,]$statistic,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_t_value =coeffs[1,]$statistic,
                sigma = stats[1,]$sigma,
                AIC = stats[1,]$AIC))
  
}

lm_part_number_sps<-function(df, threshold =0, y = "pm2.5",  bin_field, ...){
  
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  

  
  res<-lm(df[[y]] ~ df[[bin_field[1]]] + df[[bin_field[2]]]) 
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #corr <- res %>% broom::augment()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_n05 = coeffs[2,]$estimate, slope_n05_std =coeffs[2,]$std.error, slope_n05_t_value =coeffs[2,]$statistic,
                slope_n1 = coeffs[3,]$estimate, slope_n1_std =coeffs[3,]$std.error, slope_n1_t_value =coeffs[3,]$statistic,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_t_value =coeffs[1,]$statistic,
                sigma = stats[1,]$sigma,
                AIC = stats[1,]$AIC))
  
}

lm_part_number_pms<-function(df, threshold =0, y = "pm2.5", bin_field, ...){
  
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  

  
  res<-lm(df[[y]] ~ df[[bin_field[1]]] + df[[bin_field[2]]] + df[[bin_field[3]]] + df[[bin_field[4]]] + df[[bin_field[5]]] + df[[bin_field[6]]]) 
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #corr <- res %>% broom::augment()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_gr03um = coeffs[2,]$estimate, slope_gr03um_std =coeffs[2,]$std.error, slope_gr03um_t_value =coeffs[2,]$statistic,
                slope_gr05um = coeffs[3,]$estimate, slope_gr05um_std =coeffs[3,]$std.error, slope_gr05um_t_value =coeffs[3,]$statistic,
                slope_gr10um = coeffs[4,]$estimate, slope_gr10um_std =coeffs[4,]$std.error, slope_gr10um_t_value =coeffs[4,]$statistic,
                slope_gr25um = coeffs[5,]$estimate, slope_gr25um_std =coeffs[5,]$std.error, slope_gr25um_t_value =coeffs[5,]$statistic,
                slope_gr50um = coeffs[6,]$estimate, slope_gr50um_std =coeffs[6,]$std.error, slope_gr50um_t_value =coeffs[6,]$statistic,
                slope_gr100um = coeffs[7,]$estimate, slope_gr100um_std =coeffs[7,]$std.error, slope_gr100um_t_value =coeffs[7,]$statistic,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_t_value =coeffs[1,]$statistic,
                sigma = stats[1,]$sigma,
                AIC = stats[1,]$AIC))
  
}

lm_part_number_sps_rh<-function(df, threshold =0, y = "pm2.5",  rh="rh",  bin_field, ...){
 
 
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  
  #bin_names <- c("'roll_n05'", "'roll_n1'", "'roll_n25'", "'roll_n4'", "'roll_n10'")
  #df_names<-lapply(bin_names, function(x) paste0("df[[",x,"]]",collapse=""))
  #df_names<- lapply(df_names, function(x) paste0(x,"+",collapse=""))
  #formula <-paste0("df[[", y, "]]~df[[", x, "]]+df[[", rh, "]]+", paste0(df_names, collapse=""),collapse="")
  #formula<-substr(formula, 1, nchar(formula)-1)
  #formula <- as.formula(formula)
  
  res<-lm(df[[y]] ~ df[[rh]] + df[[bin_field[1]]] + df[[bin_field[2]]]) 
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #corr <- res %>% broom::augment()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_rh = coeffs[2,]$estimate, slope_rh_std =coeffs[2,]$std.error, slope_rh_t_value =coeffs[2,]$statistic,
                slope_n05 = coeffs[3,]$estimate, slope_n05_std =coeffs[3,]$std.error, slope_n05_t_value =coeffs[3,]$statistic,
                slope_n1 = coeffs[4,]$estimate, slope_n1_std =coeffs[4,]$std.error, slope_n1_t_value =coeffs[4,]$statistic,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_t_value =coeffs[1,]$statistic,
                sigma = stats[1,]$sigma,
                AIC = stats[1,]$AIC))
  
}

lm_part_number_pms_rh<-function(df, threshold =0, y = "pm2.5",  rh="rh", bin_field,  ...){  
  
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  if(is.null(df)){
    warning("inner join dimension is 0")
    return(tibble(slope = NA, slope_std = NA, slope_p_value = NA,
                  intercept = NA, intercept_std = NA, intercept_p_value = NA,
                  r_squared_adjusted = NA))
  }
  
  #bin_names <- c("'roll_n05'", "'roll_n1'", "'roll_n25'", "'roll_n4'", "'roll_n10'")
  #df_names<-lapply(bin_names, function(x) paste0("df[[",x,"]]",collapse=""))
  #df_names<- lapply(df_names, function(x) paste0(x,"+",collapse=""))
  #formula <-paste0("df[[", y, "]]~df[[", x, "]]+df[[", rh, "]]+", "df[[", temperature, "]]+", paste0(df_names, collapse=""),collapse="")
  #formula<-substr(formula, 1, nchar(formula)-1)
  #formula <- as.formula(formula)
  
  res<-lm(df[[y]] ~ df[[rh]] + df[[bin_field[1]]] + df[[bin_field[2]]] + df[[bin_field[3]]] + df[[bin_field[4]]] + df[[bin_field[5]]] + df[[bin_field[6]]]) 
  stats <-res %>% broom::glance()
  coeffs <- res %>% broom::tidy()
  #corr <- res %>% broom::augment()
  #spearman <- cor(df[[y]], df[[x]], use = "pairwise", method = "spearman")   
  return(tibble(slope_rh = coeffs[2,]$estimate, slope_rh_std =coeffs[2,]$std.error, slope_rh_t_value =coeffs[2,]$statistic,
                slope_gr03um = coeffs[3,]$estimate, slope_gr03um_std =coeffs[3,]$std.error, slope_gr03um_t_value =coeffs[3,]$statistic,
                slope_gr05um = coeffs[4,]$estimate, slope_gr05um_std =coeffs[4,]$std.error, slope_gr05um_t_value =coeffs[4,]$statistic,
                slope_gr10um = coeffs[5,]$estimate, slope_gr10um_std =coeffs[5,]$std.error, slope_gr10um_t_value =coeffs[5,]$statistic,
                slope_gr25um = coeffs[6,]$estimate, slope_gr25um_std =coeffs[6,]$std.error, slope_gr25um_t_value =coeffs[6,]$statistic,
                slope_gr50um = coeffs[7,]$estimate, slope_gr50um_std =coeffs[7,]$std.error, slope_gr50um_t_value =coeffs[7,]$statistic,
                slope_gr100um = coeffs[8,]$estimate, slope_gr100um_std =coeffs[8,]$std.error, slope_gr100um_t_value =coeffs[8,]$statistic,
                intercept = coeffs[1,]$estimate, intercept_std =coeffs[1,]$std.error, intercept_t_value =coeffs[1,]$statistic,
                sigma = stats[1,]$sigma,
                AIC = stats[1,]$AIC))
  
}


#' Calibrate sensors
#'
#' Only works for one type of sensor at a time.
#' Save the results in output_folder
#' 
#' @param df_weather_pm_train - dataset used to train models
#' @param df_weather_pm_calibration - dataset to test the models
#' @param sensor_type - type of sensor (SPS or PMS)
#' @param calibration_scenario - name of the calibration scenario tested
#' @param output_folder - output folder
#' @param method_list c("mlr1","mlr2", "koehlermass", "koehlermassrh85", "koehlermassrh80", "koehlermassrh75", "koehlersize", "koehlersizerh85", "koehlersizerh80", "koehlersizerh75", "laulainen",
#'  "rlmhuber", "rlmbisquare", "rlmhampel", "rlmrh", "rlmpartrh", "lmpart", "lmpartrh")
#'  @param sensor_field - name of the field containing the readings from the sensors
#'  @param reference_field - name of the field containing the readings from the reference method
#'  @param rh_field - name of the field containing the relative humidity data
#'  @param limit_value - limit value for the calculation of the expanded uncertainty
#'  @param rm_unceratiny - uncertainty of the reference method for the expanded uncertainty
#'  @param bin_field = c("roll_14min_median_gr03um","roll_14min_median_gr05um",
#' "roll_14min_median_gr10um","roll_14min_median_gr25um",
#'"roll_14min_median_gr50um","roll_14min_median_gr100um")
#' @return
#' @export
#'
#' @examples
calibrate_sensors <- function(df_weather_pm_train, df_weather_pm_calibration, 
                              sensor_type, calibration_scenario, 
                              output_folder, method_list="",
                              sensor_field = "roll_pm25_2min",
                              reference_field = "roll_Fidas_pm25_2min",
                              rh_field ="roll_median_humidity",
                              limit_value = 30, 
                              rm_uncertainty = 0.61,
                              bin_field = c("roll_n05","roll_n1")){
  
  df_weather_pm_train <- df_weather_pm_train[grepl(sensor_type, df_weather_pm_train$sensor),] %>%
    ungroup()
  df_weather_pm_calibration <- df_weather_pm_calibration[grepl(sensor_type, df_weather_pm_calibration$sensor),] %>%
    ungroup()
  
  if("initial" %in% method_list){ 
    message("initial")
  res <- calculate_metrics(df_weather_pm_train, sensor_field = sensor_field, reference_field = reference_field, method = "Initial train", limit_value = limit_value, 
                           rm_uncertainty = rm_uncertainty) 
  res_cal<-calculate_metrics(df_weather_pm_calibration, sensor_field = sensor_field, reference_field = reference_field, method = "Initial calibration", limit_value = limit_value, 
                           rm_uncertainty = rm_uncertainty)

  saveRDS(res, file = paste0(output_folder, "res_initial_train_", sensor_type, "_", calibration_scenario, ".rds"))
  saveRDS(res_cal, file = paste0(output_folder, "res_initial_cal_", sensor_type, "_", calibration_scenario, ".rds"))
  saveRDS(df_weather_pm_train,file = paste0(output_folder, "df_weather_pm_train_initial_", sensor_type, "_",calibration_scenario, "_train.rds"))
  saveRDS(df_weather_pm_calibration,file = paste0(output_folder, "df_weather_pm_calibration_initial_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  if("ols" %in% method_list){
    message("ols")
    res<- ols(df_weather_pm_train,sensor_field = sensor_field, reference_field = reference_field)
    
    tmp_test <- res %>%
      left_join(df_weather_pm_train, by=c("sensor"))
    tmp <- res %>%
      left_join(df_weather_pm_calibration, by=c("sensor"))
    
    tmp_test <- tmp_test %>%
      mutate(PM25_corr_ols = (.data[[sensor_field]] - a) / b ) %>%
      dplyr::select(site, sensor, date, PM25_corr_ols, .data[[reference_field]])
    tmp <- tmp %>%
      mutate(PM25_corr_ols = (.data[[sensor_field]] - a) / b )%>%
      dplyr::select(site, sensor, date, PM25_corr_ols, .data[[reference_field]])
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_ols", reference_field = reference_field, method = "ols train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_ols", reference_field = reference_field, method = "ols calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_ols_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_ols_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_ols), 
            file = paste0(output_folder, "df_weather_pm_train_ols_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_ols),  
            file = paste0(output_folder, "df_weather_pm_calibration_ols_", sensor_type, "_",calibration_scenario, ".rds"))
    
    
  }
  
  
  # Linear model no RH -------------------
  if("lr" %in% method_list){
    message("LR")
    res_lr<-df_weather_pm_train %>%
      group_by(sensor) %>%
      nest() %>%
      mutate(regression = map(data, ~clean_regression(df= .x, x=sensor_field, y=reference_field))) %>%
      dplyr::select(-data) %>%
      unnest(cols=c(regression))
    tmp_test <- res_lr %>%
      left_join(df_weather_pm_train, by=c("sensor"))
    tmp <- res_lr %>%
      left_join(df_weather_pm_calibration, by=c("sensor"))
    
    ## Correction
    tmp_test <- tmp_test %>%
      mutate(PM25_corr_LR = slope*.data[[sensor_field]]  + intercept)
    #df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_LR), by= c("sensor", "date"))
    tmp <- tmp %>%
      mutate(PM25_corr_LR = slope*.data[[sensor_field]]  + intercept)
    #df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_LR), by= c("sensor", "date"))
    
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_LR", reference_field = reference_field, method = "lr train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_LR", reference_field = reference_field, method = "lr calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_lr_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_lr_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_LR), 
            file = paste0(output_folder, "df_weather_pm_train_lr_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_LR),  file = paste0(output_folder, "df_weather_pm_calibration_lr_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  
  
  # MLR with RH ------------------------
  if("mlr1" %in% method_list){
    message("MLR RH")
    res_mlr_1<-df_weather_pm_train %>%
      group_by(sensor) %>%
      nest() %>%
      mutate(regression = map(data, ~mlr_1(df= .x, x=sensor_field, y=reference_field,rh=rh_field))) %>%
      dplyr::select(-data) %>%
      unnest(cols=c(regression))
    tmp_test <- res_mlr_1 %>%
      left_join(df_weather_pm_train, by=c("sensor"))
    tmp <- res_mlr_1 %>%
      left_join(df_weather_pm_calibration, by=c("sensor"))
    
    ## Correction
    tmp_test <- tmp_test %>%
      mutate(PM25_corr_MLR1 = slope_x*.data[[sensor_field]] + slope_rh*.data[[rh_field]] + intercept)
    #df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_MLR1), by= c("sensor", "date"))
    tmp <- tmp %>%
      mutate(PM25_corr_MLR1 = slope_x*.data[[sensor_field]] + slope_rh*.data[[rh_field]] + intercept)
    #df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_MLR1), by= c("sensor", "date"))
    
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_MLR1", reference_field = reference_field, method = "mlr1 train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_MLR1", reference_field = reference_field, method = "mlr1 calibration", limit_value = limit_value, 
                                 rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_mlr1_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_mlr1_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_MLR1), 
            file = paste0(output_folder, "df_weather_pm_train_mlr1_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_MLR1),  file = paste0(output_folder, "df_weather_pm_calibration_mlr1_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  # # MLR with RHT --------------------------------
  # # Regression
  # 
  # if("mlr2" %in% method_list){
  #   message("MLR RHT")
  #   res_mlr_2 <- df_weather_pm_train %>%
  #     group_by(sensor) %>%
  #     nest() %>%
  #     mutate(regression = map(data, ~mlr_2(df= .x, x=sensor_field, y=reference_field,rh=rh_field))) %>%
  #     dplyr::select(-data) %>%
  #     unnest(cols=c(regression))
  #   
  #   tmp<-res_mlr_2 %>%
  #     left_join(df_weather_pm_calibration, by=c("sensor"))
  #   tmp_test<-res_mlr_2 %>%
  #     left_join(df_weather_pm_train, by=c("sensor"))
  #   # Correction
  #   tmp <- tmp %>%
  #     mutate(PM25_corr_MLR2 = slope_x*.data[[sensor_field]] + slope_rh*.data[[rh_field]] + slope_temp*temperature+intercept)
  #   df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, select(tmp, sensor, date, PM25_corr_MLR2), by= c("sensor", "date"))
  #   tmp_test <- tmp_test %>%
  #     mutate(PM25_corr_MLR2 = slope_x*.data[[sensor_field]] + slope_rh*.data[[rh_field]] + slope_temp*temperature+intercept)
  #   df_weather_pm_train <- inner_join(df_weather_pm_train, select(tmp_test, sensor, date, PM25_corr_MLR2), by= c("sensor", "date"))
  # 
  #   res <- calculate_expanded_uncertainty_cv(df_weather_pm_calibration, df_weather_pm_train, res_mlr_2, 
  #                                            sensor_field = "PM25_corr_MLR2", 
  #                                            reference_field = reference_field, 
  #                                            name = "mlr2")
  #   saveRDS(res, file = paste0(output_folder, "res_mlr2_", sensor_type, "_", calibration_scenario, ".rds"))
  #   saveRDS(dplyr::select(df_weather_pm_train, date, sensor, PM25_corr_MLR2) ,file = paste0(output_folder, "df_weather_pm_train_mlr2_", sensor_type, "_",calibration_scenario, "_train.rds"))
  #   saveRDS(dplyr::select(df_weather_pm_calibration, date, sensor, PM25_corr_MLR2),file = paste0(output_folder, "df_weather_pm_calibration_mlr2_", sensor_type, "_",calibration_scenario, ".rds"))
  # }
  # 
  # k-koehler on PM mass concentration ---------------------------
  
  if("koehlermass" %in% method_list){
    message("Koehler mass")
    kappa <- 0.1 # Like in Birmingham (Crilley2020)
    rho_particles <- 1.6 # g/cm-3 like OPC-N2
    df_weather_pm_train$PM25_corr_koehler_mass = df_weather_pm_train[[sensor_field]] / (1 + (kappa/rho_particles)/(-1 + 100/df_weather_pm_train[[rh_field]]))
    df_weather_pm_calibration$PM25_corr_koehler_mass = df_weather_pm_calibration[[sensor_field]] / (1 + (kappa/rho_particles)/(-1 + 100/df_weather_pm_calibration[[rh_field]]))
    
    
    

    res <- calculate_metrics(df_weather_pm_train, sensor_field = "PM25_corr_koehler_mass", reference_field = reference_field, method = "koehlermass train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(df_weather_pm_calibration, sensor_field = "PM25_corr_koehler_mass", reference_field = reference_field, method = "koehlermass calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlermass_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlermass_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    
    saveRDS(dplyr::select(df_weather_pm_train, date, sensor, PM25_corr_koehler_mass),file = paste0(output_folder, "df_weather_pm_train_koehlermass_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(df_weather_pm_calibration, date, sensor, PM25_corr_koehler_mass),file = paste0(output_folder, "df_weather_pm_calibration_koehlermass_", sensor_type, "_",calibration_scenario, ".rds"))
    
    ## Second correction
    tmp_test <- df_weather_pm_train %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_mass_ols = (PM25_corr_koehler_mass -a)/ b) %>% #ols takes y=sensor and x=reference method
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    tmp <- df_weather_pm_calibration %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_mass_ols = (PM25_corr_koehler_mass -a)/ b ) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_mass_ols", reference_field = reference_field, method = "koehlermassols train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_mass_ols", reference_field = reference_field, method = "koehlermassols calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlermassols_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlermassols_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_mass_ols),file = paste0(output_folder, "df_weather_pm_train_koehlermassols_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_mass_ols),file = paste0(output_folder, "df_weather_pm_calibration_koehlermassols_", sensor_type, "_",calibration_scenario, ".rds"))
    
    ## Second correction LR
    res_lr<-tmp_test %>%
      group_by(sensor) %>%
      nest() %>%
      mutate(regression = map(data, ~clean_regression(df= .x, x="PM25_corr_koehler_mass", y=reference_field))) %>%
      dplyr::select(-data) %>%
      unnest(cols=c(regression))
    tmp_test <- res_lr %>%
      left_join(tmp_test, by=c("sensor"))
    tmp <- res_lr %>%
      left_join(tmp, by=c("sensor"))
    
    ### Correction
    tmp_test <- tmp_test %>%
      mutate(PM25_corr_koehler_mass_LR = slope*.data[[sensor_field]]  + intercept)
    #df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_koehler_mass_LR), by= c("sensor", "date"))
    tmp <- tmp %>%
      mutate(PM25_corr_koehler_mass_LR = slope*.data[[sensor_field]]  + intercept)
    #df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_koehler_mass_LR), by= c("sensor", "date"))
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_mass_LR", reference_field = reference_field, method = "koehlermasslr train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_mass_LR", reference_field = reference_field, method = "koehlermasslr calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlermasslr_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlermasslr_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_mass_LR),file = paste0(output_folder, "df_weather_pm_train_koehlermasslr_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_mass_LR),file = paste0(output_folder, "df_weather_pm_calibration_koehlermasslr_", sensor_type, "_",calibration_scenario, ".rds"))
    
    
  }
  # PM mass concentration  if RH>85% ------------------------------
  
  if("koehlermassrh85" %in% method_list){
    message("Koehler mass RH 85")
    kappa <- 0.1 # Like in Birmingham (Crilley2020)
    rho_particles <- 1.6 # g/cm-3 like OPC-N2
    
    tmp_test <- df_weather_pm_train %>%
      mutate(PM25_corr_koehler_mass_rh_85 = ifelse(.data[[rh_field]]>=85, .data[[sensor_field]] / (1 + (kappa/rho_particles)/(-1 + 100/.data[[rh_field]])), .data[[sensor_field]]))
    tmp <- df_weather_pm_calibration %>%
      mutate(PM25_corr_koehler_mass_rh_85 = ifelse(.data[[rh_field]]>=85, .data[[sensor_field]] / (1 + (kappa/rho_particles)/(-1 + 100/.data[[rh_field]])), .data[[sensor_field]]))
    
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_mass_rh_85", reference_field = reference_field, method = "koehlermassrh85 train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_mass_rh_85", reference_field = reference_field, method = "koehlermassrh85 calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlermassrh85_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlermassrh85_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_mass_rh_85),file = paste0(output_folder, "df_weather_pm_train_koehlermassrh85_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_mass_rh_85),file = paste0(output_folder, "df_weather_pm_calibration_koehlermassrh85_", sensor_type, "_",calibration_scenario, ".rds"))
    
    ## Second correction
    tmp_test <- tmp_test %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_mass_rh_ols_85 = (PM25_corr_koehler_mass_rh_85 - a)/ b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    tmp <- tmp %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_mass_rh_ols_85 = (PM25_corr_koehler_mass_rh_85 - a) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_mass_rh_ols_85", reference_field = reference_field, method = "koehlermassrh85ols train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_mass_rh_ols_85", reference_field = reference_field, method = "koehlermassrh85ols calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlermassrh85ols_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlermassrh85ols_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_mass_rh_ols_85),file = paste0(output_folder, "df_weather_pm_train_koehlermassrh85ols_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_mass_rh_ols_85),file = paste0(output_folder, "df_weather_pm_calibration_koehlermassrh85ols_", sensor_type, "_",calibration_scenario, ".rds"))
    
    ## Second correction LR
    res_lr<-tmp_test %>%
      group_by(sensor) %>%
      nest() %>%
      mutate(regression = map(data, ~clean_regression(df= .x, x="PM25_corr_koehler_mass_rh_85", y=reference_field))) %>%
      dplyr::select(-data) %>%
      unnest(cols=c(regression))
    tmp_test <- res_lr %>%
      left_join(tmp_test, by=c("sensor"))
    tmp <- res_lr %>%
      left_join(df_weather_pm_calibration, by=c("sensor"))
    
    ### Correction
    tmp_test <- tmp_test %>%
      mutate(PM25_corr_koehler_mass_LR_85 = slope*.data[[sensor_field]]  + intercept)
    #df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_koehler_mass_LR_85), by= c("sensor", "date"))
    tmp <- tmp %>%
      mutate(PM25_corr_koehler_mass_LR_85 = slope*.data[[sensor_field]]  + intercept)
    #df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_koehler_mass_LR_85), by= c("sensor", "date"))
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_mass_LR_85", reference_field = reference_field, method = "koehlermasslr85 train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_mass_LR_85", reference_field = reference_field, method = "koehlermasslr85 calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlermassrh85lr_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlermassrh85lr_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_mass_LR_85),file = paste0(output_folder, "df_weather_pm_train_koehlermassrh85lr_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_mass_LR_85),file = paste0(output_folder, "df_weather_pm_calibration_koehlermassrh85lr_", sensor_type, "_",calibration_scenario, ".rds"))
    
    
  
    
    
  }
  
  # PM mass concentration  if RH>80% ------------------------------
  if("koehlermassrh80" %in% method_list){
    message("Koehler mass RH 80")
    kappa <- 0.1 # Like in Birmingham (Crilley2020)
    rho_particles <- 1.6 # g/cm-3 like OPC-N2
    
    tmp_test <- df_weather_pm_train %>%
      mutate(PM25_corr_koehler_mass_rh_80 = ifelse(.data[[rh_field]]>=80, .data[[sensor_field]] / (1 + (kappa/rho_particles)/(-1 + 100/.data[[rh_field]])), .data[[sensor_field]]))
    tmp <- df_weather_pm_calibration %>%
      mutate(PM25_corr_koehler_mass_rh_80 = ifelse(.data[[rh_field]]>=80, .data[[sensor_field]] / (1 + (kappa/rho_particles)/(-1 + 100/.data[[rh_field]])), .data[[sensor_field]]))
    
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_mass_rh_80", reference_field = reference_field, method = "koehlermassrh80 train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_mass_rh_80", reference_field = reference_field, method = "koehlermassrh80 calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlermassrh80_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlermassrh80_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_mass_rh_80),file = paste0(output_folder, "df_weather_pm_train_koehlermassrh80_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_mass_rh_80),file = paste0(output_folder, "df_weather_pm_calibration_koehlermassrh80_", sensor_type, "_",calibration_scenario, ".rds"))
    
    ## Second correction
    tmp_test <- tmp_test %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_mass_rh_ols_80 = (PM25_corr_koehler_mass_rh_80 - a) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    tmp <- tmp %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_mass_rh_ols_80 = (PM25_corr_koehler_mass_rh_80 - a) /b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_mass_rh_ols_80", reference_field = reference_field, method = "koehlermassrh80ols train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_mass_rh_ols_80", reference_field = reference_field, method = "koehlermassrh80ols calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlermassrh80ols_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlermassrh80ols_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_mass_rh_ols_80),file = paste0(output_folder, "df_weather_pm_train_koehlermassrh80ols_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_mass_rh_ols_80),file = paste0(output_folder, "df_weather_pm_calibration_koehlermassrh80ols_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  
  # PM mass concentration  if RH>75% ------------------------------
  if("koehlermassrh75" %in% method_list){
    message("Koehler mass RH 75")
    kappa <- 0.1 # Like in Birmingham (Crilley2020)
    rho_particles <- 1.6 # g/cm-3 like OPC-N2
    
    tmp_test <- df_weather_pm_train %>%
      mutate(PM25_corr_koehler_mass_rh_75 = ifelse(.data[[rh_field]]>=75, .data[[sensor_field]] / (1 + (kappa/rho_particles)/(-1 + 100/.data[[rh_field]])), .data[[sensor_field]]))
    tmp <- df_weather_pm_calibration %>%
      mutate(PM25_corr_koehler_mass_rh_75 = ifelse(.data[[rh_field]]>=75, .data[[sensor_field]] / (1 + (kappa/rho_particles)/(-1 + 100/.data[[rh_field]])), .data[[sensor_field]]))
    
   
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_mass_rh_75", reference_field = reference_field, method = "koehlermassrh75 train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_mass_rh_75", reference_field = reference_field, method = "koehlermassrh75 calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlermassrh75_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlermassrh75_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_mass_rh_75),file = paste0(output_folder, "df_weather_pm_train_koehlermassrh75_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_mass_rh_75),file = paste0(output_folder, "df_weather_pm_calibration_koehlermassrh75_", sensor_type, "_",calibration_scenario, ".rds"))
    
    
    
    ## Second correction
    tmp_test <- tmp_test %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_mass_rh_ols_75 = (PM25_corr_koehler_mass_rh_75 - a) / b ) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    tmp <- tmp %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_mass_rh_ols_75 = (PM25_corr_koehler_mass_rh_75 - a) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    

    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_mass_rh_ols_75", reference_field = reference_field, method = "koehlermassrh75ols train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_mass_rh_ols_75", reference_field = reference_field, method = "koehlermassrh75ols calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlermassrh75ols_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlermassrh75ols_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_mass_rh_ols_75),file = paste0(output_folder, "df_weather_pm_train_koehlermassrh75ols_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_mass_rh_ols_75),file = paste0(output_folder, "df_weather_pm_calibration_koehlermassrh75ols_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  
  # Particle distribution --------------------------
  if("koehlersize" %in% method_list){
    kappa <- 0.1 # Like in Birmingham (Crilley2020)
    rho_particles <- 1.6 # g/cm-3 like OPC-N2
    message("Koehler part")
    if(sensor_type == "SPS"){
      tmp_test <-df_weather_pm_train[grepl("SPS", df_weather_pm_train$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_n05 = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_n1 = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size = rho_particles*pi/6*(.data[[bin_field[1]]]*d_n05^3 + (.data[[bin_field[2]]] - .data[[bin_field[1]]])*d_n1^3))
        
      tmp <-df_weather_pm_calibration[grepl("SPS", df_weather_pm_calibration$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_n05 = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_n1 = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size = rho_particles*pi/6*(.data[[bin_field[1]]]*d_n05^3 + (.data[[bin_field[2]]] - .data[[bin_field[1]]])*d_n1^3))
        
    }
    if(sensor_type == "PMS"){
      tmp_test <-df_weather_pm_train[grepl("PMS", df_weather_pm_train$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_gr03um = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr05um = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr10um = (1.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr25um = (3.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr50um = (7.5/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr100um = (10/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size = rho_particles*pi/6/100*((.data[[bin_field[1]]]-.data[[bin_field[2]]])*d_gr03um^3 + (.data[[bin_field[2]]] - .data[[bin_field[3]]])*d_gr05um^3 + (.data[[bin_field[3]]] - .data[[bin_field[4]]])*d_gr10um^3 + (.data[[bin_field[4]]] - .data[[bin_field[5]]])*d_gr25um^3 + (.data[[bin_field[5]]] - .data[[bin_field[6]]])*d_gr50um^3 + (.data[[bin_field[6]]])*d_gr100um^3))
        
      tmp <-df_weather_pm_calibration[grepl("PMS", df_weather_pm_calibration$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_gr03um = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr05um = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr10um = (1.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr25um = (3.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr50um = (7.5/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr100um = (10/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size = rho_particles*pi/6/100*((.data[[bin_field[1]]]-.data[[bin_field[2]]])*d_gr03um^3 + (.data[[bin_field[2]]] - .data[[bin_field[3]]])*d_gr05um^3 + (.data[[bin_field[3]]] - .data[[bin_field[4]]])*d_gr10um^3 + (.data[[bin_field[4]]] - .data[[bin_field[5]]])*d_gr25um^3 + (.data[[bin_field[5]]] - .data[[bin_field[6]]])*d_gr50um^3 + (.data[[bin_field[6]]])*d_gr100um^3))
        
    }
    # df_weather_pm_calibration$PM25_corr_koehler_size<-NA
    # df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, df_weather_pm_model, by=c("sensor", "date"))
    # df_weather_pm_calibration$PM25_corr_koehler_size <- df_weather_pm_calibration$PM25_corr_koehler_size.y
    # df_weather_pm_train$PM25_corr_koehler_size<-NA
    # df_weather_pm_train <- inner_join(df_weather_pm_train, df_weather_pm_model, by=c("sensor", "date"))
    # df_weather_pm_train$PM25_corr_koehler_size <- df_weather_pm_train$PM25_corr_koehler_size.y
    

    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_size", reference_field = reference_field, method = "koehlersize train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_size", reference_field = reference_field, method = "koehlersize calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlersize_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlersize_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_size),file = paste0(output_folder, "df_weather_pm_train_koehlersize_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_size),file = paste0(output_folder, "df_weather_pm_calibration_koehlersize_", sensor_type, "_",calibration_scenario, ".rds"))
    
    ## Second stage
    tmp_test <- tmp_test %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_size_ols = (PM25_corr_koehler_size - a) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    tmp <- tmp %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_size_ols = (PM25_corr_koehler_size - a) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_size_ols", reference_field = reference_field, method = "koehlersizeols train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_size_ols", reference_field = reference_field, method = "koehlersizeols calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlersizeols_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlersizeols_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_size_ols),file = paste0(output_folder, "df_weather_pm_train_koehlersizeols_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_size_ols),file = paste0(output_folder, "df_weather_pm_calibration_koehlersizeols_", sensor_type, "_",calibration_scenario, ".rds"))
    
    
    ## Second correction LR
    res_lr<-tmp_test %>%
      group_by(sensor) %>%
      nest() %>%
      mutate(regression = map(data, ~clean_regression(df= .x, x="PM25_corr_koehler_size", y=reference_field))) %>%
      dplyr::select(-data) %>%
      unnest(cols=c(regression))
    tmp_test <- res_lr %>%
      left_join(tmp_test, by=c("sensor"))
    tmp <- res_lr %>%
      left_join(tmp, by=c("sensor"))
    
    ### Correction
    tmp_test <- tmp_test %>%
      mutate(PM25_corr_koehler_size_LR = slope*.data[[sensor_field]]  + intercept)
    #df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_koehler_size_LR), by= c("sensor", "date"))
    tmp <- tmp %>%
      mutate(PM25_corr_koehler_size_LR = slope*.data[[sensor_field]]  + intercept)
    #df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_koehler_size_LR), by= c("sensor", "date"))
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_size_LR", reference_field = reference_field, method = "koehlersizelr train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_size_LR", reference_field = reference_field, method = "koehlersizelr calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlersizelr_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlersizelr_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_size_LR),file = paste0(output_folder, "df_weather_pm_train_koehlersizelr_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_size_LR),file = paste0(output_folder, "df_weather_pm_calibration_koehlersizelr_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  ### Particle distribution  with RH>85 -------------------
  if("koehlersizerh85" %in% method_list){
    kappa <- 0.1 # Like in Birmingham (Crilley2020)
    rho_particles <- 1.6 # g/cm-3 like OPC-N2
    message("Koehler part RH 85")
    if(sensor_type == "SPS"){
      tmp_test <- df_weather_pm_train[grepl("SPS", df_weather_pm_train$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_n05 = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_n1 = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh = ifelse(.data[[rh_field]]>=85,
                                                  rho_particles*pi/6*(.data[[bin_field[1]]]*d_n05^3 + (.data[[bin_field[2]]] - .data[[bin_field[1]]])*d_n1^3),
                                                  .data[[sensor_field]])) 
      tmp <- df_weather_pm_calibration[grepl("SPS", df_weather_pm_calibration$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_n05 = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_n1 = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh = ifelse(.data[[rh_field]]>=85,
                                                  rho_particles*pi/6*(.data[[bin_field[1]]]*d_n05^3 + (.data[[bin_field[2]]] - .data[[bin_field[1]]])*d_n1^3),
                                                  .data[[sensor_field]])) 
    }
    if(sensor_type == "PMS"){
      tmp_test <-df_weather_pm_train[grepl("PMS", df_weather_pm_train$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_gr03um = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr05um = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr10um = (1.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr25um = (3.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr50um = (7.5/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr100um = (10/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh = ifelse(.data[[rh_field]]>=85,
                                                  rho_particles*pi/6/100*((.data[[bin_field[1]]]-.data[[bin_field[2]]])*d_gr03um^3 + (.data[[bin_field[2]]] - .data[[bin_field[3]]])*d_gr05um^3 + (.data[[bin_field[3]]] - .data[[bin_field[4]]])*d_gr10um^3 + (.data[[bin_field[4]]] - .data[[bin_field[5]]])*d_gr25um^3 + (.data[[bin_field[5]]] - .data[[bin_field[6]]])*d_gr50um^3 + (.data[[bin_field[6]]])*d_gr100um^3),
                                                  .data[[sensor_field]])) 
      
      tmp <-df_weather_pm_calibration[grepl("PMS", df_weather_pm_calibration$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_gr03um = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr05um = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr10um = (1.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr25um = (3.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr50um = (7.5/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr100um = (10/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh = ifelse(.data[[rh_field]]>=85,
                                                  rho_particles*pi/6/100*((.data[[bin_field[1]]]-.data[[bin_field[2]]])*d_gr03um^3 + (.data[[bin_field[2]]] - .data[[bin_field[3]]])*d_gr05um^3 + (.data[[bin_field[3]]] - .data[[bin_field[4]]])*d_gr10um^3 + (.data[[bin_field[4]]] - .data[[bin_field[5]]])*d_gr25um^3 + (.data[[bin_field[5]]] - .data[[bin_field[6]]])*d_gr50um^3 + (.data[[bin_field[6]]])*d_gr100um^3),
                                                  .data[[sensor_field]]))
    }
    # df_weather_pm_calibration$PM25_corr_koehler_size_rh<-NA
    # df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, df_weather_pm_model, by=c("sensor", "date"))
    # df_weather_pm_calibration$PM25_corr_koehler_size_rh <- df_weather_pm_calibration$PM25_corr_koehler_size_rh.y
    # df_weather_pm_train$PM25_corr_koehler_size_rh<-NA
    # df_weather_pm_train <- inner_join(df_weather_pm_train, df_weather_pm_model, by=c("sensor", "date"))
    # df_weather_pm_train$PM25_corr_koehler_size_rh <- df_weather_pm_train$PM25_corr_koehler_size_rh.y
    # 
  
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_size_rh", reference_field = reference_field, method = "koehlersizerh85 train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_size_rh", reference_field = reference_field, method = "koehlersizerh85 calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlersizerh85_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlersizerh85_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_size_rh),file = paste0(output_folder, "df_weather_pm_train_koehlersizerh85_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_size_rh),file = paste0(output_folder, "df_weather_pm_calibration_koehlersizerh85_", sensor_type, "_",calibration_scenario, ".rds"))
  
  
    ## Second stage
    
    tmp <- tmp %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_size_ols_rh = (PM25_corr_koehler_size_rh - a) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    tmp_test <- tmp_test %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_size_ols_rh = (PM25_corr_koehler_size_rh - a) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    

    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_size_ols_rh", reference_field = reference_field, method = "koehlersizerh85ols train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_size_ols_rh", reference_field = reference_field, method = "koehlersizerh85ols calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlersizerh85ols_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlersizerh85ols_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_size_ols_rh),file = paste0(output_folder, "df_weather_pm_train_koehlersizerh85ols_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_size_ols_rh),file = paste0(output_folder, "df_weather_pm_calibration_koehlersizerh85ols_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  ### Particle distribution  with RH>80 -------------------
  if("koehlersizerh80" %in% method_list){
    kappa <- 0.1 # Like in Birmingham (Crilley2020)
    rho_particles <- 1.6 # g/cm-3 like OPC-N2
    message("Koehler part RH 80")
    if(sensor_type == "SPS"){
      tmp_test <- df_weather_pm_train[grepl("SPS", df_weather_pm_train$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_n05 = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_n1 = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh_80 = ifelse(.data[[rh_field]]>=80,
                                                  rho_particles*pi/6*(.data[[bin_field[1]]]*d_n05^3 + (.data[[bin_field[2]]] - .data[[bin_field[1]]])*d_n1^3),
                                                  .data[[sensor_field]])) 
      tmp <- df_weather_pm_calibration[grepl("SPS", df_weather_pm_calibration$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_n05 = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_n1 = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh_80 = ifelse(.data[[rh_field]]>=80,
                                                  rho_particles*pi/6*(.data[[bin_field[1]]]*d_n05^3 + (.data[[bin_field[2]]] - .data[[bin_field[1]]])*d_n1^3),
                                                  .data[[sensor_field]])) 
    }
    if(sensor_type == "PMS"){
      tmp_test <- df_weather_pm_train[grepl("PMS", df_weather_pm_train$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_gr03um = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr05um = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr10um = (1.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr25um = (3.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr50um = (7.5/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr100um = (10/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh_80 = ifelse(.data[[rh_field]]>=80,
                                                  rho_particles*pi/6/100*((.data[[bin_field[1]]]-.data[[bin_field[2]]])*d_gr03um^3 + (.data[[bin_field[2]]] - .data[[bin_field[3]]])*d_gr05um^3 + (.data[[bin_field[3]]] - .data[[bin_field[4]]])*d_gr10um^3 + (.data[[bin_field[4]]] - .data[[bin_field[5]]])*d_gr25um^3 + + (.data[[bin_field[5]]] - .data[[bin_field[6]]])*d_gr50um^3 + (.data[[bin_field[6]]])*d_gr100um^3),
                                                  .data[[sensor_field]])) 
      tmp <- df_weather_pm_calibration[grepl("PMS", df_weather_pm_calibration$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_gr03um = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr05um = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr10um = (1.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr25um = (3.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr50um = (7.5/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr100um = (10/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh_80 = ifelse(.data[[rh_field]]>=80,
                                                  rho_particles*pi/6/100*((.data[[bin_field[1]]]-.data[[bin_field[2]]])*d_gr03um^3 + (.data[[bin_field[2]]] - .data[[bin_field[3]]])*d_gr05um^3 + (.data[[bin_field[3]]] - .data[[bin_field[4]]])*d_gr10um^3 + (.data[[bin_field[4]]] - .data[[bin_field[5]]])*d_gr25um^3 ++ (.data[[bin_field[5]]] - .data[[bin_field[6]]])*d_gr50um^3 + (.data[[bin_field[6]]])*d_gr100um^3),
                                                  .data[[sensor_field]])) 
    }
    # df_weather_pm_calibration$PM25_corr_koehler_size_rh<-NA
    # df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, df_weather_pm_model, by=c("sensor", "date"))
    # df_weather_pm_calibration$PM25_corr_koehler_size_rh <- df_weather_pm_calibration$PM25_corr_koehler_size_rh.y
    # df_weather_pm_train$PM25_corr_koehler_size_rh<-NA
    # df_weather_pm_train <- inner_join(df_weather_pm_train, df_weather_pm_model, by=c("sensor", "date"))
    # df_weather_pm_train$PM25_corr_koehler_size_rh <- df_weather_pm_train$PM25_corr_koehler_size_rh.y
    # 

    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_size_rh_80", reference_field = reference_field, method = "koehlersizerh80 train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_size_rh_80", reference_field = reference_field, method = "koehlersizerh80 calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlersizerh80_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlersizerh80_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_size_rh_80),file = paste0(output_folder, "df_weather_pm_train_koehlersizerh80_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_size_rh_80), file = paste0(output_folder, "df_weather_pm_calibration_koehlersizerh80_", sensor_type, "_",calibration_scenario, ".rds"))
    
    ## Second stage
    
    tmp <- tmp %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_size_ols_rh_80 = (PM25_corr_koehler_size_rh_80 - a) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    tmp_test <- tmp_test %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_size_ols_rh_80 = (PM25_corr_koehler_size_rh_80 - a) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_size_ols_rh_80", reference_field = reference_field, method = "koehlersizerh80ols train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_size_ols_rh_80", reference_field = reference_field, method = "koehlersizerh80ols calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlersizerh80ols_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlersizerh80ols_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_size_ols_rh_80),file = paste0(output_folder, "df_weather_pm_train_koehlersizerh80ols_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_size_ols_rh_80),file = paste0(output_folder, "df_weather_pm_calibration_koehlersizerh80ols_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  ### Particle distribution  with RH>75 -------------------
  if("koehlersizerh75" %in% method_list){
    message("Koehler part RH 75")
    if(sensor_type == "SPS"){
      tmp_test <- df_weather_pm_train[grepl("SPS", df_weather_pm_train$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_n05 = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_n1 = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh_75 = ifelse(.data[[rh_field]]>=75,
                                                  rho_particles*pi/6*(.data[[bin_field[1]]]*d_n05^3 + (.data[[bin_field[2]]] - .data[[bin_field[1]]])*d_n1^3),
                                                  .data[[sensor_field]])) 
      tmp <- df_weather_pm_calibration[grepl("SPS", df_weather_pm_calibration$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_n05 = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_n1 = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh_75 = ifelse(.data[[rh_field]]>=75,
                                                  rho_particles*pi/6*(.data[[bin_field[1]]]*d_n05^3 + (.data[[bin_field[2]]] - .data[[bin_field[1]]])*d_n1^3),
                                                  .data[[sensor_field]])) 
    }
    if(sensor_type == "PMS"){
      tmp_test <- df_weather_pm_train[grepl("PMS", df_weather_pm_train$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_gr03um = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr05um = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr10um = (1.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr25um = (3.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr50um = (7.5/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr100um = (10/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh_75 = ifelse(.data[[rh_field]]>=75,
                                                  rho_particles*pi/6/100*((.data[[bin_field[1]]]-.data[[bin_field[2]]])*d_gr03um^3 + (.data[[bin_field[2]]] - .data[[bin_field[3]]])*d_gr05um^3 + (.data[[bin_field[3]]] - .data[[bin_field[4]]])*d_gr10um^3 + (.data[[bin_field[4]]] - .data[[bin_field[5]]])*d_gr25um^3 + (.data[[bin_field[5]]] - .data[[bin_field[6]]])*d_gr50um^3 + (.data[[bin_field[6]]])*d_gr100um^3),
                                                  .data[[sensor_field]])) 
      tmp <- df_weather_pm_calibration[grepl("PMS", df_weather_pm_calibration$sensor),] %>%
        group_by(sensor) %>%
        mutate(d_gr03um = (0.4/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr05um = (0.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr10um = (1.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr25um = (3.75/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr50um = (7.5/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3)),
               d_gr100um = (10/(1+kappa*.data[[rh_field]]/(100-.data[[rh_field]]))^(1/3))) %>%
        mutate(PM25_corr_koehler_size_rh_75 = ifelse(.data[[rh_field]]>=75,
                                                  rho_particles*pi/6/100*((.data[[bin_field[1]]]-.data[[bin_field[2]]])*d_gr03um^3 + (.data[[bin_field[2]]] - .data[[bin_field[3]]])*d_gr05um^3 + (.data[[bin_field[3]]] - .data[[bin_field[4]]])*d_gr10um^3 + (.data[[bin_field[4]]] - .data[[bin_field[5]]])*d_gr25um^3 + (.data[[bin_field[5]]] - .data[[bin_field[6]]])*d_gr50um^3 + (.data[[bin_field[6]]])*d_gr100um^3),
                                                  .data[[sensor_field]])) 
    }
    # df_weather_pm_calibration$PM25_corr_koehler_size_rh<-NA
    # df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, df_weather_pm_model, by=c("sensor", "date"))
    # df_weather_pm_calibration$PM25_corr_koehler_size_rh <- df_weather_pm_calibration$PM25_corr_koehler_size_rh.y
    # df_weather_pm_train$PM25_corr_koehler_size_rh<-NA
    # df_weather_pm_train <- inner_join(df_weather_pm_train, df_weather_pm_model, by=c("sensor", "date"))
    # df_weather_pm_train$PM25_corr_koehler_size_rh <- df_weather_pm_train$PM25_corr_koehler_size_rh.y
    
   
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_size_rh_75", reference_field = reference_field, method = "koehlersizerh75 train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_size_rh_75", reference_field = reference_field, method = "koehlersizerh75 calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlersizerh75_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlersizerh75_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_size_rh_75),file = paste0(output_folder, "df_weather_pm_train_koehlersizerh75_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_size_rh_75),file = paste0(output_folder, "df_weather_pm_calibration_koehlersizerh75_", sensor_type, "_",calibration_scenario, ".rds"))
    
    ## Second stage
    
    tmp <- tmp %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_size_ols_rh_75 = (PM25_corr_koehler_size_rh_75  -a ) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    tmp_test <- tmp_test %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_koehler_size_ols_rh_75 = (PM25_corr_koehler_size_rh_75 - a) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_koehler_size_ols_rh_75", reference_field = reference_field, method = "koehlersizerh75ols train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_koehler_size_ols_rh_75", reference_field = reference_field, method = "koehlersizerh75ols calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_koehlersizerh75_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_koehlersizerh75_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_koehler_size_ols_rh_75),file = paste0(output_folder, "df_weather_pm_train_koehlersizerh75ols_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_koehler_size_ols_rh_75),file = paste0(output_folder, "df_weather_pm_calibration_koehlersizerh75ols_", sensor_type, "_",calibration_scenario, ".rds"))
  }  
  
  
  #' Laulainen RH
  if("laulainen" %in% method_list){
    message("Laulainen")
    df_weather_pm_train$PM25_corr_Laulainen_rh = df_weather_pm_train[[sensor_field]] / (1 + 0.25*(df_weather_pm_train[[rh_field]]/100)^2/(1 - df_weather_pm_train[[rh_field]]/100))
    df_weather_pm_calibration$PM25_corr_Laulainen_rh = df_weather_pm_calibration[[sensor_field]] / (1 + 0.25*(df_weather_pm_calibration[[rh_field]]/100)^2/(1 - df_weather_pm_calibration[[rh_field]]/100))
    
    
    res <- calculate_metrics(df_weather_pm_train, sensor_field = "PM25_corr_Laulainen_rh", reference_field = reference_field, method = "laulainen train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(df_weather_pm_calibration, sensor_field = "PM25_corr_Laulainen_rh", reference_field = reference_field, method = "laulainen calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_laulainen_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_laulainen_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(df_weather_pm_train, date, sensor, PM25_corr_Laulainen_rh),file = paste0(output_folder, "df_weather_pm_train_laulainen_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(df_weather_pm_calibration, date, sensor, PM25_corr_Laulainen_rh),file = paste0(output_folder, "df_weather_pm_calibration_laulainen_", sensor_type, "_",calibration_scenario, ".rds"))
    
    
    ## Second stage
    
    tmp <- df_weather_pm_calibration %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_Laulainen_rh_ols = (PM25_corr_Laulainen_rh  -a ) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    tmp_test <- df_weather_pm_train %>%
      right_join(res$res[res$res$type == "Full", ], by=c("sensor")) %>%
      mutate(PM25_corr_Laulainen_rh_ols = (PM25_corr_Laulainen_rh - a) / b) %>%
      dplyr::select(-names(res$res[res$res$type == "Full", ]), sensor)
    
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_Laulainen_rh_ols", reference_field = reference_field, method = "laulainenols train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_Laulainen_rh_ols", reference_field = reference_field, method = "laulainenols calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_laulainenols_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_laulainenols_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_Laulainen_rh_ols),file = paste0(output_folder, "df_weather_pm_train_laulainenols_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_Laulainen_rh_ols),file = paste0(output_folder, "df_weather_pm_calibration_laulainenols_", sensor_type, "_",calibration_scenario, ".rds"))
    
    
    ## Second correction LR
    res_lr<-tmp_test %>%
      group_by(sensor) %>%
      nest() %>%
      mutate(regression = map(data, ~clean_regression(df= .x, x="PM25_corr_Laulainen_rh", y=reference_field))) %>%
      dplyr::select(-data) %>%
      unnest(cols=c(regression))
    tmp_test <- res_lr %>%
      left_join(tmp_test, by=c("sensor"))
    tmp <- res_lr %>%
      left_join(tmp, by=c("sensor"))
    
    ### Correction
    tmp_test <- tmp_test %>%
      mutate(PM25_corr_Laulainen_rh_LR = slope*.data[[sensor_field]]  + intercept)
   # df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_Laulainen_rh_LR), by= c("sensor", "date"))
    tmp <- tmp %>%
      mutate(PM25_corr_Laulainen_rh_LR = slope*.data[[sensor_field]]  + intercept)
   # df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_Laulainen_rh_LR), by= c("sensor", "date"))
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_Laulainen_rh_LR", reference_field = reference_field, method = "laulainenlr train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_Laulainen_rh_LR", reference_field = reference_field, method = "laulainenlr calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_laulainenlr_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_laulainenlr_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_Laulainen_rh_LR),file = paste0(output_folder, "df_weather_pm_train_laulainenlr_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_Laulainen_rh_LR),file = paste0(output_folder, "df_weather_pm_calibration_laulainenlr_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  
  # Robust RLM ------------------
  if("rlmhuber" %in% method_list){
    message("Robust RLM - Huber")
    ## Huber psi
    ### Regression
    res_rlm<-df_weather_pm_train %>%
      group_by(sensor) %>%
      nest() %>%
      mutate(regression = map(data, ~rlm_nest(df= .x, x=sensor_field, y=reference_field, psi_function = psi.huber))) %>%
      dplyr::select(-data) %>%
      unnest(cols=c(regression))
    
    tmp_test<-res_rlm %>%
      left_join(df_weather_pm_train, by=c("sensor"))
    tmp<-res_rlm %>%
      left_join(df_weather_pm_calibration, by=c("sensor"))
    ### Correction
    tmp<- tmp %>%
      mutate(PM25_corr_RLM = slope_x*.data[[sensor_field]] + intercept)
    #df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_RLM), by= c("sensor", "date"))
    tmp_test<- tmp_test %>%
      mutate(PM25_corr_RLM = slope_x*.data[[sensor_field]] + intercept)
    #df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_RLM), by= c("sensor", "date"))
 
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_RLM", reference_field = reference_field, method = "rlmhuber train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_RLM", reference_field = reference_field, method = "rlmhuber calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_rlmhuber_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_rlmhuber_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_RLM),file = paste0(output_folder, "df_weather_pm_train_rlmhuber_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_RLM),file = paste0(output_folder, "df_weather_pm_calibration_rlmhuber_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  ## Bisquare psi
  if("rlmbisquare" %in% method_list){
    message("Robust RLM - Bisquare")
    ### Regression
    res_rlm_bisquare<-df_weather_pm_train %>%
      group_by(sensor) %>%
      nest() %>%
      mutate(regression = map(data, ~rlm_nest(df= .x, x=sensor_field, y=reference_field, psi_function = psi.bisquare))) %>%
      dplyr::select(-data) %>%
      unnest(cols=c(regression))
    
    tmp_test<-res_rlm_bisquare %>%
      left_join(df_weather_pm_train, by=c("sensor"))
    tmp<-res_rlm_bisquare %>%
      left_join(df_weather_pm_calibration, by=c("sensor"))
    ### Correction
    tmp<- tmp %>%
      mutate(PM25_corr_RLM_bisquare = slope_x*.data[[sensor_field]] + intercept)
  #  df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_RLM_bisquare), by= c("sensor", "date"))
    tmp_test<- tmp_test %>%
      mutate(PM25_corr_RLM_bisquare = slope_x*.data[[sensor_field]] + intercept)
   # df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_RLM_bisquare), by= c("sensor", "date"))
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_RLM_bisquare", reference_field = reference_field, method = "rlmbisquare train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_RLM_bisquare", reference_field = reference_field, method = "rlmbisquare calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_rlmbisquare_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_rlmbisquare_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_RLM_bisquare),file = paste0(output_folder, "df_weather_pm_train_rlmbisquare_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_RLM_bisquare),file = paste0(output_folder, "df_weather_pm_calibration_rlmbisquare_", sensor_type, "_",calibration_scenario, ".rds"))
  }  
  ## Hampel psi
  if("rlmhampel" %in% method_list){
    message("Robust RLM - Hampel")
    ### Regression
    res_rlm_hampel<-df_weather_pm_train %>%
      group_by(sensor) %>%
      nest() %>%
      mutate(regression = map(data, ~rlm_nest(df= .x, x=sensor_field, y=reference_field, psi_function = psi.hampel))) %>%
      dplyr::select(-data) %>%
      unnest(cols=c(regression))
    
    tmp_test<-res_rlm_hampel %>%
      left_join(df_weather_pm_train, by=c("sensor"))
    tmp<-res_rlm_hampel %>%
      left_join(df_weather_pm_calibration, by=c("sensor"))
    ### Correction
    tmp<- tmp %>%
      mutate(PM25_corr_RLM_hampel = slope_x*.data[[sensor_field]] + intercept)
  #  df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_RLM_hampel), by= c("sensor", "date"))
    tmp_test<- tmp_test %>%
      mutate(PM25_corr_RLM_hampel = slope_x*.data[[sensor_field]] + intercept)
   # df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_RLM_hampel), by= c("sensor", "date"))
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_RLM_hampel", reference_field = reference_field, method = "rlmhampel train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_RLM_hampel", reference_field = reference_field, method = "rlmhampel calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_rlmhampel_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_rlmhampel_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_RLM_hampel),file = paste0(output_folder, "df_weather_pm_train_rlmhampel_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_RLM_hampel),file = paste0(output_folder, "df_weather_pm_calibration_rlmhampel_", sensor_type, "_",calibration_scenario, ".rds"))
  } 
  ## Huber with RH
  if("rlmrh" %in% method_list){
    message("Robust RLM - Huber and RH")
    res_rlm_rh <- df_weather_pm_train %>%
      group_by(sensor) %>%
      nest() %>%
      mutate(regression = map(data, ~rlm_rh_nest(df= .x, x=sensor_field, y=reference_field, rh=rh_field,psi_function = psi.huber))) %>%
      dplyr::select(-data) %>%
      unnest(cols=c(regression))
    tmp_test<-res_rlm_rh %>%
      left_join(df_weather_pm_train, by=c("sensor"))
    tmp<-res_rlm_rh %>%
      left_join(df_weather_pm_calibration, by=c("sensor"))
    ### Correction
    tmp <- tmp %>%
      mutate(PM25_corr_RLM_rh = slope_x*.data[[sensor_field]] + intercept + .data[[rh_field]]*slope_rh)
 #   df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_RLM_rh), by= c("sensor", "date"))
    tmp_test <- tmp_test %>%
      mutate(PM25_corr_RLM_rh = slope_x*.data[[sensor_field]] + intercept + .data[[rh_field]]*slope_rh)
   # df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_RLM_rh), by= c("sensor", "date"))
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_RLM_rh", reference_field = reference_field, method = "rlmrh train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_RLM_rh", reference_field = reference_field, method = "rlmrh calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_rlmrh_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_rlmrh_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_RLM_rh),file = paste0(output_folder, "df_weather_pm_train_rlmrh_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_RLM_rh),file = paste0(output_folder, "df_weather_pm_calibration_rlmrh_", sensor_type, "_",calibration_scenario, ".rds"))
  } 
  
  ## Huber with RH on part. number
  if("rlmpartrh" %in% method_list){
    message("Robust RLM - Huber and RH Part number")
    if(sensor_type=="SPS"){
      res_rlm_rh <- df_weather_pm_train %>%
        group_by(sensor) %>%
        nest() %>%
        mutate(regression = map(data, ~rlm_part_rh_sps_nest(df= .x, x=sensor_field, y=reference_field, rh=rh_field,bin_field = bin_field, psi_function = psi.huber))) %>%
        dplyr::select(-data) %>%
        unnest(cols=c(regression))
      tmp_test<-res_rlm_rh %>%
        left_join(df_weather_pm_train, by=c("sensor"))
      tmp<-res_rlm_rh %>%
        left_join(df_weather_pm_calibration, by=c("sensor"))
      ### Correction
      tmp <- tmp %>%
        mutate(PM25_corr_RLM_rh_part = intercept + .data[[rh_field]]*slope_rh + .data[[bin_field[1]]]*slope_n05 + .data[[bin_field[2]]] * slope_n1)
     # df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_RLM_rh_part), by= c("sensor", "date"))
      tmp_test <- tmp_test %>%
        mutate(PM25_corr_RLM_rh_part = intercept + .data[[rh_field]]*slope_rh + .data[[bin_field[1]]]*slope_n05 + .data[[bin_field[2]]] * slope_n1)
     # df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_RLM_rh_part), by= c("sensor", "date"))
    }
    if(sensor_type == "PMS"){
      res_rlm_rh <- df_weather_pm_train %>%
        group_by(sensor) %>%
        nest() %>%
        mutate(regression = map(data, ~rlm_part_rh_pms_nest(df= .x, x=sensor_field, y=reference_field, rh=rh_field, bin_field = bin_field, psi_function = psi.huber))) %>%
        dplyr::select(-data) %>%
        unnest(cols=c(regression))
      tmp_test<-res_rlm_rh %>%
        left_join(df_weather_pm_train, by=c("sensor"))
      tmp<-res_rlm_rh %>%
        left_join(df_weather_pm_calibration, by=c("sensor"))
      ### Correction
      tmp <- tmp %>%
        mutate(PM25_corr_RLM_rh_part = intercept + .data[[rh_field]]*slope_rh + .data[[bin_field[1]]]*slope_gr03um + .data[[bin_field[2]]] * slope_gr05um + .data[[bin_field[3]]] * slope_gr10um + .data[[bin_field[4]]] * slope_gr25um + .data[[bin_field[5]]] * slope_gr50um + .data[[bin_field[6]]] * slope_gr100um)
      #df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_RLM_rh_part), by= c("sensor", "date"))
      tmp_test <- tmp_test %>%
        mutate(PM25_corr_RLM_rh_part = intercept + .data[[rh_field]]*slope_rh + .data[[bin_field[1]]]*slope_gr03um + .data[[bin_field[2]]] * slope_gr05um + .data[[bin_field[3]]] * slope_gr10um + .data[[bin_field[4]]] * slope_gr25um + .data[[bin_field[5]]] * slope_gr50um + .data[[bin_field[6]]] * slope_gr100um)
     # df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_RLM_rh_part), by= c("sensor", "date"))
    }
 
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_RLM_rh_part", reference_field = reference_field, method = "rlmpartrh train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_RLM_rh_part", reference_field = reference_field, method = "rlmpartrh calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    saveRDS(res, file = paste0(output_folder, "res_rlmpartrh_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_rlmpartrh_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_RLM_rh_part),file = paste0(output_folder, "df_weather_pm_train_rlmpartrh_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_RLM_rh_part),file = paste0(output_folder, "df_weather_pm_calibration_rlmpartrh_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  
  ## Huber on part. number
  if("rlmpart" %in% method_list){
    message("Robust RLM - Huber Part number")
    if(sensor_type=="SPS"){
      res_rlm <- df_weather_pm_train %>%
        group_by(sensor) %>%
        nest() %>%
        mutate(regression = map(data, ~rlm_part_sps_nest(df= .x, x=sensor_field, y=reference_field, bin_field = bin_field, psi_function = psi.huber))) %>%
        dplyr::select(-data) %>%
        unnest(cols=c(regression))
      tmp_test<-res_rlm %>%
        left_join(df_weather_pm_train, by=c("sensor"))
      tmp<-res_rlm %>%
        left_join(df_weather_pm_calibration, by=c("sensor"))
      ### Correction
      tmp <- tmp %>%
        mutate(PM25_corr_RLM_part = intercept + .data[[bin_field[1]]]*slope_n05 + .data[[bin_field[2]]] * slope_n1)
      # df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_RLM_rh_part), by= c("sensor", "date"))
      tmp_test <- tmp_test %>%
        mutate(PM25_corr_RLM_part = intercept  + .data[[bin_field[1]]]*slope_n05 + .data[[bin_field[2]]] * slope_n1)
      # df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_RLM_rh_part), by= c("sensor", "date"))
    }
    if(sensor_type == "PMS"){
      res_rlm <- df_weather_pm_train %>%
        group_by(sensor) %>%
        nest() %>%
        mutate(regression = map(data, ~rlm_part_pms_nest(df= .x, x=sensor_field, y=reference_field, bin_field = bin_field, psi_function = psi.huber))) %>%
        dplyr::select(-data) %>%
        unnest(cols=c(regression))
      tmp_test<-res_rlm %>%
        left_join(df_weather_pm_train, by=c("sensor"))
      tmp<-res_rlm %>%
        left_join(df_weather_pm_calibration, by=c("sensor"))
      ### Correction
      tmp <- tmp %>%
        mutate(PM25_corr_RLM_part = intercept +  .data[[bin_field[1]]]*slope_gr03um + .data[[bin_field[2]]] * slope_gr05um + .data[[bin_field[3]]] * slope_gr10um + .data[[bin_field[4]]] * slope_gr25um + .data[[bin_field[5]]] * slope_gr50um + .data[[bin_field[6]]] * slope_gr100um)
      #df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_RLM_rh_part), by= c("sensor", "date"))
      tmp_test <- tmp_test %>%
        mutate(PM25_corr_RLM_part = intercept +  .data[[bin_field[1]]]*slope_gr03um + .data[[bin_field[2]]] * slope_gr05um + .data[[bin_field[3]]] * slope_gr10um + .data[[bin_field[4]]] * slope_gr25um + .data[[bin_field[5]]] * slope_gr50um + .data[[bin_field[6]]] * slope_gr100um)
      # df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_RLM_rh_part), by= c("sensor", "date"))
    }
    
    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_RLM_part", reference_field = reference_field, method = "rlmpart train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_RLM_part", reference_field = reference_field, method = "rlmpart calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    saveRDS(res, file = paste0(output_folder, "res_rlmpart_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_rlmpart_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_RLM_part),file = paste0(output_folder, "df_weather_pm_train_rlmpart_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_RLM_part),file = paste0(output_folder, "df_weather_pm_calibration_rlmpart_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  # LM on particle number -----------------------------
  if("lmpart" %in% method_list){
    message("LM part number")
    ## Regression
    if(sensor_type=="SPS"){
      res_lm_part_number <- df_weather_pm_train %>%
        group_by(sensor) %>%
        nest() %>%
        mutate(regression = map(data, ~lm_part_number_sps(df= .x, y=reference_field, bin_field = bin_field))) %>%
        dplyr::select(-data) %>%
        unnest(cols=c(regression))
      tmp<-res_lm_part_number%>%
        left_join(df_weather_pm_calibration, by=c("sensor"))
      tmp_test<-res_lm_part_number %>%
        left_join(df_weather_pm_train, by=c("sensor"))
      ## Correction
      tmp <- tmp %>%
        mutate(PM25_corr_lm_part_number = intercept + .data[[bin_field[1]]]*slope_n05 + .data[[bin_field[2]]] * slope_n1)
      #df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_lm_part_number), by= c("sensor", "date"))
      tmp_test <- tmp_test %>%
        mutate(PM25_corr_lm_part_number = intercept + .data[[bin_field[1]]]*slope_n05 + .data[[bin_field[2]]] * slope_n1)
      #df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_lm_part_number), by= c("sensor", "date"))
    }
    if(sensor_type == "PMS"){
      res_lm_part_number <- df_weather_pm_train %>%
        group_by(sensor) %>%
        nest() %>%
        mutate(regression = map(data, ~lm_part_number_pms(df= .x, x=sensor_field, y=reference_field, bin_field = bin_field))) %>%
        dplyr::select(-data) %>%
        unnest(cols=c(regression))
      tmp<-res_lm_part_number%>%
        left_join(df_weather_pm_calibration, by=c("sensor"))
      tmp_test<-res_lm_part_number %>%
        left_join(df_weather_pm_train, by=c("sensor"))
      ## Correction
      tmp <- tmp %>%
        mutate(PM25_corr_lm_part_number = intercept + .data[[bin_field[1]]]*slope_gr03um + .data[[bin_field[2]]] * slope_gr05um + .data[[bin_field[3]]] * slope_gr10um + .data[[bin_field[4]]] * slope_gr25um + .data[[bin_field[5]]] * slope_gr50um + .data[[bin_field[6]]] * slope_gr100um)
     # df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_lm_part_number), by= c("sensor", "date"))
      tmp_test <- tmp_test %>%
        mutate(PM25_corr_lm_part_number = intercept + .data[[bin_field[1]]]*slope_gr03um + .data[[bin_field[2]]] * slope_gr05um + .data[[bin_field[3]]] * slope_gr10um + .data[[bin_field[4]]] * slope_gr25um + .data[[bin_field[5]]] * slope_gr50um + .data[[bin_field[6]]] * slope_gr100um)
     # df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_lm_part_number), by= c("sensor", "date"))
    }

    res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_lm_part_number", reference_field = reference_field, method = "lmpart train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_lm_part_number", reference_field = reference_field, method = "lmpart calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_lmpart_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_lmpart_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_lm_part_number),file = paste0(output_folder, "df_weather_pm_train_lmpart_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_lm_part_number),file = paste0(output_folder, "df_weather_pm_calibration_lmpart_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  # LM on particle number  with RH ----------------------
  if("lmpartrh" %in% method_list){
    message("LM part number with RH")
    # Regression
    if(sensor_type=="SPS"){
        res_lm_part_number_rh <- df_weather_pm_train %>%
          group_by(sensor) %>%
          nest() %>%
          mutate(regression = map(data, ~lm_part_number_sps_rh(df= .x, x=sensor_field, y=reference_field, rh = rh_field, bin_field = bin_field))) %>%
          dplyr::select(-data) %>%
          unnest(cols=c(regression))
        
        tmp<-res_lm_part_number_rh %>%
          left_join(df_weather_pm_calibration, by=c("sensor"))
        tmp_test<-res_lm_part_number_rh %>%
          left_join(df_weather_pm_train, by=c("sensor"))
        # Correction
        tmp <- tmp %>%
          mutate(PM25_corr_lm_part_number_rh = intercept + .data[[rh_field]]*slope_rh + .data[[bin_field[1]]]*slope_n05 + .data[[bin_field[2]]] * slope_n1)
       # df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_lm_part_number_rh), by= c("sensor", "date"))
        
        tmp_test <- tmp_test %>%
          mutate(PM25_corr_lm_part_number_rh = intercept + .data[[rh_field]]*slope_rh + .data[[bin_field[1]]]*slope_n05 + .data[[bin_field[2]]] * slope_n1)
       # df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_lm_part_number_rh), by= c("sensor", "date"))
      }
      if(sensor_type == "PMS"){
        res_lm_part_number_rh <- df_weather_pm_train %>%
          group_by(sensor) %>%
          nest() %>%
          mutate(regression = map(data, ~lm_part_number_pms_rh(df= .x, x=sensor_field, y=reference_field, rh=rh_field, bin_field = bin_field))) %>%
          dplyr::select(-data) %>%
          unnest(cols=c(regression))
        
        tmp<-res_lm_part_number_rh %>%
          left_join(df_weather_pm_calibration, by=c("sensor"))
        tmp_test<-res_lm_part_number_rh %>%
          left_join(df_weather_pm_train, by=c("sensor"))
        ## Correction
        tmp <- tmp %>%
          mutate(PM25_corr_lm_part_number_rh = intercept + .data[[rh_field]]*slope_rh + .data[[bin_field[1]]]*slope_gr03um + .data[[bin_field[2]]] * slope_gr05um + .data[[bin_field[3]]] * slope_gr10um + .data[[bin_field[4]]] * slope_gr25um + .data[[bin_field[5]]] * slope_gr50um + .data[[bin_field[6]]] * slope_gr100um)
       # df_weather_pm_calibration <- inner_join(df_weather_pm_calibration, dplyr::select(tmp, sensor, date, PM25_corr_lm_part_number_rh), by= c("sensor", "date"))
        
        tmp_test <- tmp_test %>%
          mutate(PM25_corr_lm_part_number_rh = intercept + .data[[rh_field]]*slope_rh + .data[[bin_field[1]]]*slope_gr03um + .data[[bin_field[2]]] * slope_gr05um + .data[[bin_field[3]]] * slope_gr10um + .data[[bin_field[4]]] * slope_gr25um + .data[[bin_field[5]]] * slope_gr50um + .data[[bin_field[6]]] * slope_gr100um)
       # df_weather_pm_train <- inner_join(df_weather_pm_train, dplyr::select(tmp_test, sensor, date, PM25_corr_lm_part_number_rh), by= c("sensor", "date"))
      }
  
      res <- calculate_metrics(tmp_test, sensor_field = "PM25_corr_lm_part_number_rh", reference_field = reference_field, method = "lmpartrh train", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
      res_cal<-calculate_metrics(tmp, sensor_field = "PM25_corr_lm_part_number_rh", reference_field = reference_field, method = "lmpartrh calibration", limit_value = limit_value, 
                                 rm_uncertainty = rm_uncertainty)
      
      
      saveRDS(res, file = paste0(output_folder, "res_lmpartrh_train_", sensor_type, "_", calibration_scenario, ".rds"))
      saveRDS(res_cal, file = paste0(output_folder, "res_lmpartrh_cal_", sensor_type, "_", calibration_scenario, ".rds"))
      saveRDS(dplyr::select(tmp_test, date, sensor, PM25_corr_lm_part_number_rh),file = paste0(output_folder, "df_weather_pm_train_lmpartrh_", sensor_type, "_",calibration_scenario, "_train.rds"))
      saveRDS(dplyr::select(tmp, date, sensor, PM25_corr_lm_part_number_rh),file = paste0(output_folder, "df_weather_pm_calibration_lmpartrh_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  
  
}




calibrate_ml_sensors <- function(df_weather_pm_train, df_weather_pm_calibration, sensor_type, calibration_scenario, output_folder, method_list ="",
                                 sensor_field = "roll_pm25_2min",
                                 reference_field = "roll_Fidas_pm25_2min",
                                 rh_field ="roll_median_humidity",
                                 limit_value = 30, 
                                 rm_uncertainty = 0.61,
                                 bin_field = c("roll_n05","roll_n1")){
# Methods implemented c("svm", "svmpart", "svmpartrh", "gbm", "gbmrh", "gbmpart", "gbmpartrh")
  # SVR ------------------------------------------------
  if("svm" %in% method_list){
    message(date())
    message("SVM")
    df_weather_pm_train$PM25_corr_svm <- double(length = nrow(df_weather_pm_train))
    df_weather_pm_calibration$PM25_corr_svm <- double(length = nrow(df_weather_pm_calibration))
    
    for(sensor in unique(df_weather_pm_train$sensor)){
      message(sensor)
      df_sensor_train <- df_weather_pm_train[df_weather_pm_train$sensor == sensor,] %>%
        dplyr::select(.data[[sensor_field]], .data[[reference_field]]) 
      df_sensor <- df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor,] %>%
        dplyr::select(.data[[sensor_field]], .data[[reference_field]]) 
      
      model_svm <- svm(get(reference_field) ~ get(sensor_field),
                       data=df_sensor_train,
                       type = "eps-regression",
                       kernel="radial",
                       cost=10,
                       gamma=10^(-2))
      
      
      corrected_test <-predict(model_svm, 
                               na.exclude(df_sensor_train))
      # data.frame(roll_pm25_2min=df_sensor_test[[sensor_field]],
      #            roll_median_humidity=df_sensor_test[[rh_field]],
      #            roll_Fidas_pm25_2min=df_sensor_test[[reference_field]]), na.action = na.exclude
      corrected<-predict(model_svm,na.exclude(df_sensor))
      df_weather_pm_train[df_weather_pm_train$sensor == sensor, ]$PM25_corr_svm <- corrected_test
      df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ]$PM25_corr_svm <- corrected
      #saveRDS(model_svm, paste0("output/calibration_models/", sensor, "_", calibration_scenario, "_svm.rds"))
      
      
    }
    
    
    res <- calculate_metrics(df_weather_pm_train, sensor_field = "PM25_corr_svm", reference_field = reference_field, method = "svm train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(df_weather_pm_calibration, sensor_field = "PM25_corr_svm", reference_field = reference_field, method = "svm calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_svm_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_svm_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(df_weather_pm_train, date, sensor, PM25_corr_svm),file = paste0(output_folder, "df_weather_pm_train_svm_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(df_weather_pm_calibration, date, sensor, PM25_corr_svm),file = paste0(output_folder, "df_weather_pm_calibration_svm_", sensor_type, "_",calibration_scenario, ".rds"))
  }      
  
  
  # SVR RH------------------------------------------------
  if("svmrh" %in% method_list){
    message(date())
    message("SVM RH")
    df_weather_pm_train$PM25_corr_svmrh <- double(length = nrow(df_weather_pm_train))
    df_weather_pm_calibration$PM25_corr_svmrh <- double(length = nrow(df_weather_pm_calibration))
    
    for(sensor in unique(df_weather_pm_train$sensor)){
      message(sensor)
      df_sensor_train <- df_weather_pm_train[df_weather_pm_train$sensor == sensor,] %>%
        dplyr::select(.data[[sensor_field]], .data[[reference_field]], .data[[rh_field]]) 
      df_sensor <- df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor,] %>%
        dplyr::select(.data[[sensor_field]], .data[[reference_field]], .data[[rh_field]]) 
      
      model_svm <- svm(get(reference_field) ~ get(sensor_field)+get(rh_field),
                       data=df_sensor_train,
                       type = "eps-regression",
                       kernel="radial",
                       cost=10,
                       gamma=10^(-2))
      
      
      corrected_test <-predict(model_svm, 
                               na.exclude(df_sensor_train))
      # data.frame(roll_pm25_2min=df_sensor_test[[sensor_field]],
      #            roll_median_humidity=df_sensor_test[[rh_field]],
      #            roll_Fidas_pm25_2min=df_sensor_test[[reference_field]]), na.action = na.exclude
      corrected<-predict(model_svm,na.exclude(df_sensor))
      df_weather_pm_train[df_weather_pm_train$sensor == sensor, ]$PM25_corr_svmrh <- corrected_test
      df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ]$PM25_corr_svmrh <- corrected
      #saveRDS(model_svm, paste0("output/calibration_models/", sensor, "_", calibration_scenario, "_svm.rds"))
      
      
    }
    

    res <- calculate_metrics(df_weather_pm_train, sensor_field = "PM25_corr_svmrh", reference_field = reference_field, method = "svmrh train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(df_weather_pm_calibration, sensor_field = "PM25_corr_svmrh", reference_field = reference_field, method = "svmrh calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_svmrh_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_svmrh_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(df_weather_pm_train, date, sensor, PM25_corr_svmrh),file = paste0(output_folder, "df_weather_pm_train_svmrh_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(df_weather_pm_calibration, date, sensor, PM25_corr_svmrh),file = paste0(output_folder, "df_weather_pm_calibration_svmrh_", sensor_type, "_",calibration_scenario, ".rds"))
  }      


  # SVR with particle number ------------------------------------------------
  if("svmpart" %in% method_list){
    message(date())
    message("SVM Part")
    df_weather_pm_train$PM25_corr_svm_part <- double(length = nrow(df_weather_pm_train))
    df_weather_pm_calibration$PM25_corr_svm_part <- double(length = nrow(df_weather_pm_calibration))
    
    for(sensor in unique(df_weather_pm_train$sensor)){
      message(sensor)
      df_sensor_train <- df_weather_pm_train[df_weather_pm_train$sensor == sensor,]
      df_sensor <- df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor,]
      
      if(sensor_type == "SPS"){
        df_train_short<-df_sensor_train %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]],.data[[bin_field[1]]] ,.data[[bin_field[2]]])
        df_short<-df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ] %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]],.data[[bin_field[1]]] ,.data[[bin_field[2]]])
        model_svm_part <- svm(get(reference_field)~., 
                              data=df_train_short,
                              type = "eps-regression",
                              kernel="radial",
                              cost=10,
                              gamma=10^(-2))
      }
      if(sensor_type == "PMS"){
        df_train_short<-df_sensor_train %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]], .data[[bin_field[2]]], .data[[bin_field[4]]], .data[[bin_field[3]]], .data[[bin_field[1]]], .data[[bin_field[5]]], .data[[bin_field[6]]])
        df_short<-df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ] %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]], .data[[bin_field[2]]], .data[[bin_field[4]]], .data[[bin_field[3]]], .data[[bin_field[1]]], .data[[bin_field[5]]], .data[[bin_field[6]]])
        model_svm_part <- svm(get(reference_field)~get(bin_field[2]) + get(bin_field[4]) + get(bin_field[3]) + get(bin_field[1]) + get(bin_field[5]) + get(bin_field[6]), 
                              data=df_train_short,
                              type = "eps-regression",
                              kernel="radial",
                              cost=10,
                              gamma=10^(-2))
        
      }
      corrected_test <-predict(model_svm_part, df_train_short, na.action = na.exclude)
      corrected<-predict(model_svm_part, df_short, na.action = na.exclude)
      df_weather_pm_train[df_weather_pm_train$sensor == sensor, ]$PM25_corr_svm_part <- corrected_test
      df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ]$PM25_corr_svm_part <- corrected
      #saveRDS(model_svm, paste0("output/calibration_models/", sensor, "_", calibration_scenario, "_svm_part_number.rds"))
      
      
    }
    
    res <- calculate_metrics(df_weather_pm_train, sensor_field = "PM25_corr_svm_part", reference_field = reference_field, method = "svmpart train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(df_weather_pm_calibration, sensor_field = "PM25_corr_svm_part", reference_field = reference_field, method = "svmpart calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_svmpart_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_svmpart_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(df_weather_pm_train, date, sensor, PM25_corr_svm_part),file = paste0(output_folder, "df_weather_pm_train_svmpart_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(df_weather_pm_calibration, date, sensor, PM25_corr_svm_part),file = paste0(output_folder, "df_weather_pm_calibration_svmpart_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  # SVR with particle number and rh------------------------------------------------
  if("svmpartrh" %in% method_list){
    message(date())
    message("SVM Part RH")
    df_weather_pm_train$PM25_corr_svm_part_rh <- double(length = nrow(df_weather_pm_train))
    df_weather_pm_calibration$PM25_corr_svm_part_rh <- double(length = nrow(df_weather_pm_calibration))
    
    for(sensor in unique(df_weather_pm_train$sensor)){
      message(sensor)
      df_sensor_train <- df_weather_pm_train[df_weather_pm_train$sensor == sensor,]
      df_sensor <- df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor,]
      
      if(sensor_type == "SPS"){
        df_train_short<-df_sensor_train %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]],.data[[bin_field[1]]] ,.data[[bin_field[2]]],.data[[rh_field]])
        df_short<-df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ] %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]],.data[[bin_field[1]]] ,.data[[bin_field[2]]],.data[[rh_field]])
        model_svm_part_rh <- svm(get(reference_field)~., 
                                 data=df_train_short,
                                 type = "eps-regression",
                                 kernel="radial",
                                 cost=10,
                                 gamma=10^(-2))
      }
      if(sensor_type == "PMS"){
        df_train_short<-df_sensor_train %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]], .data[[bin_field[2]]], .data[[bin_field[4]]], .data[[bin_field[3]]], .data[[bin_field[1]]], .data[[bin_field[5]]], .data[[bin_field[6]]],.data[[rh_field]])
        df_short<-df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ] %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]], .data[[bin_field[2]]], .data[[bin_field[4]]], .data[[bin_field[3]]], .data[[bin_field[1]]], .data[[bin_field[5]]], .data[[bin_field[6]]],.data[[rh_field]])
        model_svm_part_rh <- svm(get(reference_field)~get(bin_field[2]) + get(bin_field[4]) + get(bin_field[3]) + get(bin_field[1]) + get(bin_field[5]) + get(bin_field[6]) + get(rh_field), 
                                 data=df_train_short,
                                 type = "eps-regression",
                                 kernel="radial",
                                 cost=1000,
                                 gamma=10^(-2))
        
      }
      corrected_test <-predict(model_svm_part_rh, df_train_short, na.action = na.exclude)
      corrected<-predict(model_svm_part_rh, df_short, na.action = na.exclude)
      df_weather_pm_train[df_weather_pm_train$sensor == sensor, ]$PM25_corr_svm_part_rh <- corrected_test
      df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ]$PM25_corr_svm_part_rh <- corrected
      #saveRDS(model_svm_part_rh, paste0("output/calibration_models/", sensor, "_", calibration_scenario, "_svm_part_number_rh.rds"))
      
      
    }
    
    res <- calculate_metrics(df_weather_pm_train, sensor_field = "PM25_corr_svm_part_rh", reference_field = reference_field, method = "svmpartrh train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(df_weather_pm_calibration, sensor_field = "PM25_corr_svm_part_rh", reference_field = reference_field, method = "svmpartrh calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_svmpartrh_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_svmpartrh_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(df_weather_pm_train, date, sensor, PM25_corr_svm_part_rh),file = paste0(output_folder, "df_weather_pm_train_svmpartrh_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(df_weather_pm_calibration, date, sensor, PM25_corr_svm_part_rh),file = paste0(output_folder, "df_weather_pm_calibration_svmpartrh_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  # GBRT----------------------------------------
  if("gbm" %in% method_list){
    message(date())
    message("GBM - no cross validation")
    df_weather_pm_train$PM25_corr_gbm <- double(length = nrow(df_weather_pm_train))
    df_weather_pm_calibration$PM25_corr_gbm <- double(length = nrow(df_weather_pm_calibration))

    
    for(sensor in unique(df_weather_pm_train$sensor)){
      message(sensor)
      df_train_short <- df_weather_pm_train[df_weather_pm_train$sensor == sensor,] %>%
        ungroup() %>%
        dplyr::select(.data[[reference_field]],.data[[sensor_field]]) %>%
        na.exclude()
      df_short<-df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor,] %>%
        ungroup() %>%
        dplyr::select(.data[[reference_field]],.data[[sensor_field]]) %>%
        na.exclude()
      
      model_gbm <- gbm(get(reference_field)~., 
                       data=df_train_short , distribution = "gaussian",  # SSE loss function
                          n.trees = 1000,
                          shrinkage = 0.1,
                          interaction.depth = 6,
                          n.minobsinnode = 10)
      corrected_test<- predict(model_gbm,df_train_short)
      corrected<- predict(model_gbm,df_short)
      
      df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor &
                                  !is.na(df_weather_pm_calibration[[sensor_field]]) &
                                  ! is.na(df_weather_pm_calibration[[reference_field]]),]$PM25_corr_gbm <- corrected
      
      
      df_weather_pm_train[df_weather_pm_train$sensor == sensor &
                                  !is.na(df_weather_pm_train[[sensor_field]]) &
                                  ! is.na(df_weather_pm_train[[reference_field]]),]$PM25_corr_gbm <- corrected_test
      
      #saveRDS(model_gbm_rh, paste0("output/calibration_models/", sensor, "_", calibration_scenario, "_gbm.rds"))
    }
    
    
    res <- calculate_metrics(df_weather_pm_train, sensor_field = "PM25_corr_gbm", reference_field = reference_field, method = "gbm train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(df_weather_pm_calibration, sensor_field = "PM25_corr_gbm", reference_field = reference_field, method = "gbm calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_gbm_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_gbm_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(df_weather_pm_train, date, sensor, PM25_corr_gbm),file = paste0(output_folder, "df_weather_pm_calibration_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(df_weather_pm_calibration, date, sensor, PM25_corr_gbm),file = paste0(output_folder, "df_weather_pm_calibration_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  # GBRT with RH----------------------------------------
  if("gbmrh" %in% method_list){
    message(date())
    message("GBM RH")
    df_weather_pm_train$PM25_corr_gbm_rh <- double(length = nrow(df_weather_pm_train))
    df_weather_pm_calibration$PM25_corr_gbm_rh <- double(length = nrow(df_weather_pm_calibration))
    
    for(sensor in unique(df_weather_pm_train$sensor)){
      message(sensor)
      df_train_short <- df_weather_pm_train[df_weather_pm_train$sensor == sensor,] %>%
        ungroup() %>%
        dplyr::select(.data[[reference_field]],.data[[sensor_field]],.data[[rh_field]]) %>%
        na.exclude()
      df_short<-df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ] %>%
        ungroup() %>%
        dplyr::select(.data[[reference_field]],.data[[sensor_field]],.data[[rh_field]])%>%
        na.exclude()
      
      model_gbm_rh <- gbm(get(reference_field) ~ ., 
                          data=df_train_short , distribution = "gaussian",  # SSE loss function
                          n.trees = 1000,
                          shrinkage = 0.1,
                          interaction.depth = 6,
                          n.minobsinnode = 10,
                          cv.folds = 10)
      corrected_test<- predict(model_gbm_rh,df_train_short, na.action = na.exclude)
      corrected<- predict(model_gbm_rh,df_short, na.action = na.exclude)
      df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor &
                                  !is.na(df_weather_pm_calibration[[sensor_field]]) &
                                  ! is.na(df_weather_pm_calibration[[reference_field]]),]$PM25_corr_gbm_rh <- corrected
      
      
      df_weather_pm_train[df_weather_pm_train$sensor == sensor &
                            !is.na(df_weather_pm_train[[sensor_field]]) &
                            ! is.na(df_weather_pm_train[[reference_field]]),]$PM25_corr_gbm_rh <- corrected_test
      #saveRDS(model_gbm_rh, paste0("output/calibration_models/", sensor, "_", calibration_scenario, "_gbm.rds"))
    }

    res <- calculate_metrics(df_weather_pm_train, sensor_field = "PM25_corr_gbm_rh", reference_field = reference_field, method = "gbmrh train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(df_weather_pm_calibration, sensor_field = "PM25_corr_gbm_rh", reference_field = reference_field, method = "gbmrh calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    
    saveRDS(res, file = paste0(output_folder, "res_gbmrh_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_gbmrh_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(df_weather_pm_train, date, sensor, PM25_corr_gbm_rh),file = paste0(output_folder, "df_weather_pm_train_gbmrh_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(df_weather_pm_calibration, date, sensor, PM25_corr_gbm_rh),file = paste0(output_folder, "df_weather_pm_calibration_gbmrh_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  # GBM on particle number -----------------------------
  if("gbmpart" %in% method_list){
    message(date())
    message("GBM Part")
    df_weather_pm_train$PM25_corr_gbm_part <- double(length = nrow(df_weather_pm_train))
    df_weather_pm_calibration$PM25_corr_gbm_part <- double(length = nrow(df_weather_pm_calibration))

    
    for(sensor in unique(df_weather_pm_train$sensor)){
    message(sensor)  
      df_sensor_train <- df_weather_pm_train[df_weather_pm_train$sensor == sensor,]
      
      if(sensor_type == "SPS"){
        df_train_short<-df_sensor_train %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]],.data[[bin_field[1]]] ,.data[[bin_field[2]]]) %>%
          na.exclude()
        df_short<-df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ] %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]],.data[[bin_field[1]]] ,.data[[bin_field[2]]]) %>%
          na.exclude()
        model_gbm_part <- gbm(get(reference_field)~., 
                              data=df_train_short , distribution = "gaussian",  # SSE loss function
                              n.trees = 1000,
                              shrinkage = 0.1,
                              interaction.depth = 6,
                              n.minobsinnode = 10,
                              cv.folds = 10)
        corrected_test<- predict(model_gbm_part,df_train_short, na.action = na.exclude)
        corrected<- predict(model_gbm_part,df_short, na.action = na.exclude)
        df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor &
                                    !is.na(df_weather_pm_calibration[[sensor_field]]) &
                                    ! is.na(df_weather_pm_calibration[[reference_field]]),]$PM25_corr_gbm_part <- corrected
        
        
        df_weather_pm_train[df_weather_pm_train$sensor == sensor &
                              !is.na(df_weather_pm_train[[sensor_field]]) &
                              ! is.na(df_weather_pm_train[[reference_field]]),]$PM25_corr_gbm_part <- corrected_test
        
      }
      if(sensor_type == "PMS"){
        df_train_short<-df_sensor_train %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]], .data[[bin_field[2]]], .data[[bin_field[4]]], .data[[bin_field[3]]], .data[[bin_field[1]]], .data[[bin_field[5]]], .data[[bin_field[6]]]) %>%
          na.exclude()
        df_short<-df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ] %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]], .data[[bin_field[2]]], .data[[bin_field[4]]], .data[[bin_field[3]]], .data[[bin_field[1]]], .data[[bin_field[5]]], .data[[bin_field[6]]]) %>%
          na.exclude
        model_gbm_part <- gbm(get(reference_field)~., 
                              data=df_train_short , distribution = "gaussian",  # SSE loss function
                              n.trees = 1000,
                              shrinkage = 0.1,
                              interaction.depth = 6,
                              n.minobsinnode = 10,
                              cv.folds = 10)
        corrected_test<- predict(model_gbm_part,df_train_short, na.action = na.exclude)
        corrected<- predict(model_gbm_part,df_short, na.action = na.exclude)
        df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor &
                                    !is.na(df_weather_pm_calibration[[sensor_field]]) &
                                    ! is.na(df_weather_pm_calibration[[reference_field]]),]$PM25_corr_gbm_part <- corrected
        
        
        df_weather_pm_train[df_weather_pm_train$sensor == sensor &
                              !is.na(df_weather_pm_train[[sensor_field]]) &
                              ! is.na(df_weather_pm_train[[reference_field]]),]$PM25_corr_gbm_part <- corrected_test
      }

    }
    
    res <- calculate_metrics(df_weather_pm_train, sensor_field = "PM25_corr_gbm_part", reference_field = reference_field, method = "gbmpart train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(df_weather_pm_calibration, sensor_field = "PM25_corr_gbm_part", reference_field = reference_field, method = "gbmpart calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    saveRDS(res, file = paste0(output_folder, "res_gbmpart_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_gbmpart_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(df_weather_pm_train, date, sensor, PM25_corr_gbm_part),file = paste0(output_folder, "df_weather_pm_train_gbmpart_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(df_weather_pm_calibration, date, sensor, PM25_corr_gbm_part),file = paste0(output_folder, "df_weather_pm_calibration_gbmpart_", sensor_type, "_",calibration_scenario, ".rds"))
  }
  
  # GBM on particle number with RH -----------------------------
  if("gbmpartrh" %in% method_list){
    message(date())
    message("GBM Part RH")
    df_weather_pm_train$PM25_corr_gbm_part_rh <- double(length = nrow(df_weather_pm_train))
    df_weather_pm_calibration$PM25_corr_gbm_part_rh <- double(length = nrow(df_weather_pm_calibration))
    
    for(sensor in unique(df_weather_pm_train$sensor)){
      
      df_sensor_train <- df_weather_pm_train[df_weather_pm_train$sensor == sensor,]
      
      if(sensor_type == "SPS"){
        df_train_short<-df_sensor_train %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]],.data[[bin_field[1]]],.data[[bin_field[2]]], .data[[rh_field]]) %>%
          na.exclude
        df_short<-df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ] %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]],.data[[bin_field[1]]], .data[[bin_field[2]]], .data[[rh_field]]) %>%
          na.exclude
        model_gbm_part_rh <- gbm(get(reference_field)~., 
                                 data=df_train_short , distribution = "gaussian",  # SSE loss function
                                 n.trees = 1000,
                                 shrinkage = 0.1,
                                 interaction.depth = 6,
                                 n.minobsinnode = 10,
                                 cv.folds = 10)
        corrected_test<- predict(model_gbm_part_rh,df_train_short, na.action = na.exclude)
        corrected<- predict(model_gbm_part_rh,df_short, na.action = na.exclude)
        df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor &
                                    complete.cases(df_weather_pm_calibration[,c(reference_field,bin_field[1],bin_field[2],rh_field)])]$PM25_corr_gbm_part_rh <- corrected
        
        
        df_weather_pm_train[df_weather_pm_train$sensor == sensor &
                              complete.cases(df_weather_pm_train[,c(reference_field,bin_field[1],bin_field[2],rh_field)]),]$PM25_corr_gbm_part_rh <- corrected_test
      }
      if(sensor_type == "PMS"){
        df_train_short<-df_sensor_train %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]],.data[[bin_field[2]]] ,.data[[bin_field[4]]], .data[[bin_field[3]]], .data[[bin_field[1]]], .data[[bin_field[5]]], .data[[bin_field[6]]], .data[[rh_field]]) %>%
          na.exclude
        df_short<-df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor, ] %>%
          ungroup() %>%
          dplyr::select(.data[[reference_field]],.data[[bin_field[2]]] ,.data[[bin_field[4]]], .data[[bin_field[3]]], .data[[bin_field[1]]], .data[[bin_field[5]]], .data[[bin_field[6]]], .data[[rh_field]]) %>%
          na.exclude
        model_gbm_part_rh <- gbm(get(reference_field)~., 
                                 data=df_train_short , distribution = "gaussian",  # SSE loss function
                                 n.trees = 1000,
                                 shrinkage = 0.1,
                                 interaction.depth = 6,
                                 n.minobsinnode = 10,
                                 cv.folds = 10)
        corrected_test<- predict(model_gbm_part_rh,df_train_short, na.action = na.exclude)
        corrected<- predict(model_gbm_part_rh,df_short, na.action = na.exclude)
        df_weather_pm_calibration[df_weather_pm_calibration$sensor == sensor &
                                    complete.cases(df_weather_pm_calibration[,c(reference_field,bin_field[1],bin_field[2],bin_field[3],bin_field[4],bin_field[5],bin_field[6],rh_field)]),]$PM25_corr_gbm_part_rh <- corrected
        
        
        df_weather_pm_train[df_weather_pm_train$sensor == sensor &
                              complete.cases(df_weather_pm_train[,c(reference_field,bin_field[1],bin_field[2],bin_field[3],bin_field[4],bin_field[5],bin_field[6],rh_field)]),]$PM25_corr_gbm_part_rh <- corrected_test
        
      }

    }

    res <- calculate_metrics(df_weather_pm_train, sensor_field = "PM25_corr_gbm_part_rh", reference_field = reference_field, method = "gbmpartrh train", limit_value = limit_value, 
                             rm_uncertainty = rm_uncertainty)
    res_cal<-calculate_metrics(df_weather_pm_calibration, sensor_field = "PM25_corr_gbm_part_rh", reference_field = reference_field, method = "gbmpartrh calibration", limit_value = limit_value, 
                               rm_uncertainty = rm_uncertainty)
    
    saveRDS(res, file = paste0(output_folder, "res_gbmpartrh_train_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(res_cal, file = paste0(output_folder, "res_gbmpartrh_cal_", sensor_type, "_", calibration_scenario, ".rds"))
    saveRDS(dplyr::select(df_weather_pm_train, date, sensor, PM25_corr_gbm_part_rh),file = paste0(output_folder, "df_weather_pm_train_gbmpartrh_", sensor_type, "_",calibration_scenario, "_train.rds"))
    saveRDS(dplyr::select(df_weather_pm_calibration, date, sensor, PM25_corr_gbm_part_rh),file = paste0(output_folder, "df_weather_pm_calibration_gbmpartrh_", sensor_type, "_",calibration_scenario, ".rds"))
  }
}
