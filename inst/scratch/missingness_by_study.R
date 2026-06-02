suppressPackageStartupMessages({
  library(tidyverse)
  library(public.ctn0094data)
  library(conflicted)
  library(here)
  conflicts_prefer(dplyr::filter)
  options(dplyr.summarise.inform = FALSE)
})
source(here("vignettes/building_analysis.R"))

# Study sizes
cat("=== Study sizes ===\n")
print(count(analysis_tibble, project))
cat("\n")

# All variables with any NA
na_vars <- names(analysis_tibble)[sapply(analysis_tibble, function(x) any(is.na(x)))]
cat("Variables with NAs:", paste(na_vars, collapse = ", "), "\n\n")

# Missingness by study for each NA variable
results <- map_dfr(na_vars, function(col) {
  analysis_tibble |>
    group_by(project) |>
    summarise(
      n_total = n(),
      n_na    = sum(is.na(.data[[col]])),
      pct_na  = round(mean(is.na(.data[[col]])) * 100, 1),
      .groups = "drop"
    ) |>
    mutate(column = col)
})

# Wide format: one row per variable, columns per study
study_labels <- sort(unique(as.character(analysis_tibble$project)))
wide <- results |>
  pivot_wider(
    id_cols    = column,
    names_from = project,
    values_from = c(n_na, pct_na),
    names_glue = "{.value}_p{project}"
  )

cat("=== NA counts and pct by study ===\n")
print(as.data.frame(wide), row.names = FALSE)

# Save CSV
write_csv(wide, here("inst/scratch/missingness_by_study.csv"))
cat("\nSaved to inst/scratch/missingness_by_study.csv\n")
