source("R/utilities/utilities.R")
source("R/utilities/nested_models.r")
source("R/utilities/variables.r")

input_folder <-
  paste0(here::here(),
         "/output/calibration/scenarios/2weeks_24weeks_2weeks/")
output_folder <-
  paste0(here::here(),
         "/output/calibration/scenarios/demo_equivalence/")

daily_metrics(
  input_folder = input_folder,
  output_folder = output_folder,
  scenario_affix = "2w24w2w",
  initial_df_location = DF_JOINED
)