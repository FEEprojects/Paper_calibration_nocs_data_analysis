#' Aggregates the results from calibration_2weeks_40days.R
#' to be plotted by evaluation_calibration_2weeks_40days.rmd
#'


source("R/utilities/utilities.R")
source("R/utilities/nested_models.r")
source("R/utilities/utilities_plot.r")
output_folder <- paste0(here::here(), "/output/calibration/2weeks_40days_agg/")


bind_results <- function(file_list) {
  #file_list
  res <- data.frame()
  res_bus <- data.frame()
  
  for (file in file_list) {
    name <- strsplit(file, "/")[[1]][8]
    scenario <- sub(".rds", "", strsplit(name, "_")[[1]][[5]])
    method <- strsplit(name, "_")[[1]][[2]]
    dataset_type <- strsplit(name, "_")[[1]][[3]]
    tmp <- readRDS(file)
    model <- tmp$res
    model$scenario <- scenario
    model$dataset_type <- dataset_type
    #model$method <- method
    res <- bind_rows(res, model)
    model <- tmp$res_bus
    model$scenario <- scenario
    model$dataset_type <- dataset_type
    #model$method <- method
    res_bus <- bind_rows(res_bus, model)
  }
  return(list(res = res, res_bus = res_bus))
}


file_list <-
  list.files(
    path = "output/calibration/2weeks_40days/",
    pattern = "res_.*",
    full.names = T,
    recursive = T
  )


tmp <- bind_results(file_list)
tmp$res$scenario_type <- "2w"
tmp$res_bus$scenario_type <- "2w"
res <- tmp$res
res_bus <- tmp$res_bus

res <- res %>%
  mutate(method = str_split(method, " ", simplify = T)[, 1])
res_bus <- res_bus %>%
  mutate(method = str_split(method, " ", simplify = T)[, 1])

res$method <-
  fct_recode(
    res$method,
    `Koehler_mass+LR` = "koehlermasslr",
    `Koehler_mass+OLS` = "koehlermassols",
    `Koehler_size+LR` = "koehlersizelr",
    `Koehler_size+OLS` =  "koehlersizeols",
    `Laulainen+LR` = "laulainenlr",
    `Laulainen+OLS` = "laulainenols",
    `MLR_part.` =  "lmpart",
    `MLR_part.+RH` =   "lmpartrh",
    `LR` = "lr",
    `MLR_RH` = "mlr1" ,
    `OLS` =  "ols",
    `RLM` = "rlmhuber",
    `RLM_part.+RH`  = "rlmpartrh",
    `RLM+RH` =      "rlmrh",
    `RLM_part.` = "rlmpart"
  )


res$method <-
  fct_relevel(
    res$method,
    "Initial",
    "OLS",
    "LR",
    "MLR_RH",
    "MLR_part.",
    "MLR_part.+RH",
    "RLM",
    "RLM+RH",
    "RLM_part.",
    "RLM_part.+RH",
    "Koehler_mass+LR",
    "Koehler_mass+OLS",
    "Koehler_size+LR",
    "Koehler_size+OLS",
    "Laulainen+LR",
    "Laulainen+OLS"
  )



res_bus$method <-
  fct_recode(
    res_bus$method,
    `Koehler_mass+LR` = "koehlermasslr",
    `Koehler_mass+OLS` = "koehlermassols",
    `Koehler_size+LR` = "koehlersizelr",
    `Koehler_size+OLS` =  "koehlersizeols",
    `Laulainen+LR` = "laulainenlr",
    `Laulainen+OLS` = "laulainenols",
    `MLR_part.` =  "lmpart",
    `MLR_part.+RH` =   "lmpartrh",
    `LR` = "lr",
    `MLR_RH` = "mlr1" ,
    `OLS` =  "ols",
    `RLM` = "rlmhuber",
    `RLM_part.+RH`  = "rlmpartrh",
    `RLM+RH` =      "rlmrh",
    `RLM_part.` = "rlmpart"
  )


res_bus$method <-
  fct_relevel(
    res_bus$method,
    "Initial",
    "OLS",
    "LR",
    "MLR_RH",
    "MLR_part.",
    "MLR_part.+RH",
    "RLM",
    "RLM+RH",
    "RLM_part.",
    "RLM_part.+RH",
    "Koehler_mass+LR",
    "Koehler_mass+OLS",
    "Koehler_size+LR",
    "Koehler_size+OLS",
    "Laulainen+LR",
    "Laulainen+OLS"
  )


res %>% head


saveRDS(
  res,
  paste0(output_folder, "res_2weeks_40days_eu_agg.rds")
)
saveRDS(
  res_bus,
  paste0(output_folder, "res_2weeks_40days_bus_agg.rds")
)
