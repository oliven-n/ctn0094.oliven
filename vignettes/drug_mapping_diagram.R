# vignettes/drug_mapping_diagram.R
# Diagram: all_drugs (windowed raw names) -> all_drugs_filtered (21 surviving
# categories) + independent tlfb list colored by match. All lists derived live
# from public.ctn0094data; nothing hard-coded. See
# docs/superpowers/plans/2026-06-11-drug-mapping-diagram.md
suppressPackageStartupMessages({
  library(public.ctn0094data)
  library(tidyverse)
  library(scales)
  library(grid)
})

# ---- window + grouping machinery (verbatim logic from building_analysis.R) ----
drug_map <- c(
  "Acetaminophen"="Analgesic","Codeine"="Opioid","Hydrocodone"="Opioid",
  "Hydromorphone"="Opioid","Merperidine"="Opioid","Morphine"="Opioid",
  "Nalbuphine"="Opioid","Opium"="Opioid","Oxycodone"="Opioid",
  "Oxymorphone"="Opioid","Propoxyphene"="Opioid","Tramadol"="Opioid",
  "Sedative-Hypnotic"="Sedatives","Barbiturate"="Sedatives",
  "Trazodone"="Antidepressant","Tryclic-Antidepressant"="Antidepressant",
  "Gabapentin"="Analgesic","Thc"="Cannabinoids","K2"="Cannabinoids",
  "Musclerelax"="Muscle Relaxant","Mdma"="MDMA/Hallucinogen",
  "Hallucinogen"="MDMA/Hallucinogen","Heavy Drinking"="Alcohol Heavy Amnt",
  "Light Drinking"="Alcohol Light Amnt","Alcohol"="Alcohol Missing Amnt"
)

day_zero_lookup <- screening_date |>
  select(who, day_zero) |>
  mutate(day_zero = replace_na(day_zero, 0L))

in_window <- function(df) {
  df |>
    left_join(day_zero_lookup, by = "who") |>
    mutate(day_zero = replace_na(day_zero, 0L)) |>  # defensive: participants absent from screening_date get NA from left_join
    filter(when >= day_zero - 28, when < day_zero)
}

# ---- all_drugs: windowed raw names + grouping ----
all_drugs_windowed <- all_drugs |>
  mutate(what_grouped = case_when(
    what %in% names(drug_map) ~ drug_map[as.character(what)],
    .default = as.character(what)
  )) |>
  in_window()

surviving <- all_drugs_windowed |>
  count(what_grouped) |>
  filter(n >= 10) |>
  arrange(desc(n))                      # filtered-box order: by event count

src_map <- all_drugs_windowed |>        # source `what` -> surviving category
  filter(what_grouped %in% surviving$what_grouped) |>
  distinct(what_grouped, what) |>
  mutate(what = as.character(what))

axed_raw <- all_drugs_windowed |>       # windowed names whose category was axed
  filter(!what_grouped %in% surviving$what_grouped) |>
  distinct(what) |>
  mutate(what = as.character(what)) |>
  pull(what) |>
  sort()

# ---- tlfb: windowed names with count >= 1 ----
tlfb_windowed <- tlfb |> in_window()
tlfb_drugs <- tlfb_windowed |>
  count(what, name = "events") |>
  filter(events >= 1) |>
  arrange(desc(events)) |>
  mutate(what = as.character(what))

# ---- INTEGRITY CHECKS: derived lists must match the tibbles ----
stopifnot(
  "all_drugs windowed names != 42"      = n_distinct(all_drugs_windowed$what) == 42L,
  "surviving categories != 21"          = nrow(surviving) == 21L,
  "grouped+axed source rows != 42"      = nrow(src_map) + length(axed_raw) == 42L,
  "tlfb windowed drugs != 25"           = nrow(tlfb_drugs) == 25L,
  "expected multi-source groups"        = setequal(
    src_map |> count(what_grouped) |> filter(n >= 2) |> pull(what_grouped),
    c("Opioid","Cannabinoids","MDMA/Hallucinogen","Analgesic"))
)
cat("Task 1 integrity checks PASSED\n")
