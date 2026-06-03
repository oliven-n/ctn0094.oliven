# data-raw/analysis_tibble.R
# Builds and saves the analysis_tibble package dataset.
# Run manually with: source("data-raw/analysis_tibble.R")
library(conflicted)
suppressPackageStartupMessages(library(tidyverse))
library(public.ctn0094data)
library(CTNote)   # provides the outcomesCTN0094 dataset (lazy-loaded)
conflicts_prefer(dplyr::filter)

# building_analysis.R creates analysis_tibble (and analysis_base) in the env.
source(here::here("vignettes/building_analysis.R"))

# Join the modeled relapse outcome (lee2018_rel_event), matching logistic_regression.R.
analysis_tibble <- analysis_tibble |>
  dplyr::left_join(
    outcomesCTN0094 |> dplyr::select(who, outcome = lee2018_rel_event),
    by = "who"
  ) |>
  dplyr::mutate(outcome = factor(outcome, levels = c(0, 1)))

usethis::use_data(analysis_tibble, overwrite = TRUE)
