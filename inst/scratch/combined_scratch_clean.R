# ════════════════════════════════════════════════════════════════════
# combined_scratch_clean.R
# Combined and cleaned exploratory analysis drawn from:
#   - Scratch_EDA.R
#   - exploratory_analysis.R
# Reordered for logical flow. Erroneous code removed. Comments
# wrapped at 79 characters. Do not source this file as a script;
# it is scratch/interactive work.
# ════════════════════════════════════════════════════════════════════

# ── Setup ─────────────────────────────────────────────────────────────

# Install the CTN data package from GitHub (run once):
# remotes::install_github("CTN-0094/public.ctn0094data")

library(conflicted)
suppressPackageStartupMessages(library(tidyverse))
library(ggplot2)
library(patchwork)   # for multi-panel plots
library(public.ctn0094data)
conflicts_prefer(dplyr::filter)

packageDescription("public.ctn0094data")

# Creates a pop-up listing every dataset in the package
data(package = "public.ctn0094data")


# ── Everybody ─────────────────────────────────────────────────────────

glimpse(everybody)
# 3560 rows, 2 cols: "who" <int> and "project" <fct>

# Confirm uniqueness of "who"
everybody |> nrow()                  # 3560
everybody |> distinct() |> nrow()   # 3560 — all unique

everybody |> summarize(
  min_id       = min(who),
  max_id       = max(who),
  unique_count = n_distinct(who)
)
# IDs run 1–3560, all unique, nothing messy

# Count participants per project (CTN study)
everybody |> group_by(project) |> summarize(count = n())
# project 27 (CTN-0028): 1920
# project 30 (CTN-0030): 868
# project 51 (CTN-0051): 772


# ── Demographics ──────────────────────────────────────────────────────

glimpse(demographics)
# 9 cols: who <int>, age <dbl>, is_hispanic, race, job,
#         is_living_stable, education, marital, is_male
# All factor except who and age.

# ── age ──

demographics |> ggplot(aes(x = age)) + geom_histogram()

# Colored by project
demographics |>
  left_join(everybody, by = "who") |>
  ggplot(aes(x = age, fill = project)) +
  geom_histogram(color = "white") +
  scale_fill_viridis_d()

# Faceted by project — most informative for comparison
demographics |>
  left_join(everybody, by = "who") |>
  ggplot(aes(x = age)) +
  geom_histogram() +
  facet_wrap(~project)

# ── is_hispanic ──
# Study 30 is ~5% Hispanic; studies 27 and 51 are ~17%

demographics |>
  left_join(everybody, by = "who") |>
  select(who, project, is_hispanic) |>
  mutate(is_hispanic = tolower(is_hispanic)) |>
  mutate(
    hispanic_binary = case_match(
      is_hispanic,
      "yes" ~ 1,
      "no"  ~ 0,
      .default = 0
    )
  ) |>
  group_by(project) |>
  summarize(
    hispanic_count     = sum(hispanic_binary),
    total_participants = n(),
    prop_hispanic      = hispanic_count / total_participants
  )

demographics |>
  left_join(everybody, by = "who") |>
  select(who, project, is_hispanic) |>
  group_by(project) |>
  summarize(count = n())

# ── race ──

demographics |>
  select(who, race) |>
  group_by(race) |>
  summarize(count = n())
# Four categories: "Black", "Other", "Refused/missing", "White"

# Confirm totals sum to 3560
demographics |>
  select(who, race) |>
  group_by(race) |>
  summarize(count = n()) |>
  summarize(total_sum = sum(count, na.rm = TRUE))

race_by_project <- demographics |>
  left_join(everybody, by = "who") |>
  select(who, project, race) |>
  group_by(project, race) |>
  summarize(count = n(), .groups = "drop")
# .groups = "drop" prevents silent grouping in downstream calls

race_by_project

ggplot(race_by_project, aes(x = "", y = count, fill = race)) +
  geom_bar(stat = "identity", width = 1) +
  labs(title = "Race Distribution by Project") +
  facet_wrap(~project, scales = "free") +
  coord_polar(theta = "y") +
  theme_void()


# ── All Drugs ─────────────────────────────────────────────────────────

# Study medications for reference:
# CTN-0028: Methadone vs Buprenorphine-Naloxone
# CTN-0030: Only Buprenorphine-Naloxone (Suboxone)
# CTN-0051: XR Naltrexone vs Buprenorphine-Naloxone

glimpse(all_drugs)
# 307,523 rows; cols: who, what, source, when

# Count of each drug type across the full dataset (n=53 categories)
all_drugs |> group_by(what) |> count() |> print(n = 53)

# Drug grouping map. Collapses rare / related opioids and
# sedatives into groups. Keeps clinically distinct drugs separate
# (heroin, fentanyl, cocaine, crack, methamphetamine, buprenorphine,
# suboxone, methadone, benzodiazepine, etc.).
drug_map <- c(
  # Prescription opioids → "Opioid" group
  # (heroin/fentanyl/buprenorphine/suboxone kept separate)
  "Acetaminophen"          = "Opioid",
  "Codeine"                = "Opioid",
  "Hydrocodone"            = "Opioid",
  "Hydromorphone"          = "Opioid",
  "Merperidine"            = "Opioid",
  "Morphine"               = "Opioid",
  "Nalbuphine"             = "Opioid",
  "Opium"                  = "Opioid",
  "Oxycodone"              = "Opioid",
  "Oxymorphone"            = "Opioid",
  "Propoxyphene"           = "Opioid",
  "Tramadol"               = "Opioid",

  # Non-benzo sedatives → "Sedatives"
  "Sedative-Hypnotic"      = "Sedatives",
  "Barbiturate"            = "Sedatives",

  # Prescription antidepressants → "Antidepressant"
  "Trazodone"              = "Antidepressant",
  "Tryclic-Antidepressant" = "Antidepressant",

  # Gabapentin → "Analgesic"
  "Gabapentin"             = "Analgesic",

  # Naming / grouping cleanup
  "Thc"                    = "Cannabinoids",
  "K2"                     = "Cannabinoids",
  "Musclerelax"            = "Muscle Relaxant",
  "Mdma"                   = "MDMA/Hallucinogen",
  "Hallucinogen"           = "MDMA/Hallucinogen",
  "Alcohol"                = "Alcohol Missing Amnt"
)

all_drugs_grouped <- all_drugs |>
  mutate(
    what_grouped = ifelse(
      what %in% names(drug_map),
      drug_map[as.character(what)],
      as.character(what)
    ),
    what_grouped = factor(what_grouped)
  )

# Filter to the pre-study window: days -28 (inclusive) to 0
# (exclusive). Primary filter: drop categories with < 10 total
# use events — too sparse to form a useful feature.
drugs_to_keep <- all_drugs_grouped |>
  filter(when < 0, when >= -28) |>
  count(what_grouped) |>
  filter(n >= 10) |>
  pull(what_grouped)

all_drugs_filtered <- all_drugs_grouped |>
  filter(when < 0, when >= -28) |>
  filter(what_grouped %in% drugs_to_keep)

# Secondary filter: drop illicit categories used by < 1% of
# participants (near-zero-variance binary; no ML signal).
# Prescribed / legal categories are exempt — even rare use
# carries signal for medication adherence and comorbidity.
prescribed_or_legal <- c(
  "Buprenorphine", "Suboxone", "Methadone",
  "Benzodiazepine", "Sedatives", "Opioid",
  "Muscle Relaxant", "Analgesic", "Antidepressant", "Antiemetic",
  "Heavy Drinking", "Light Drinking", "Alcohol Missing Amnt",
  "Cannabinoids", "Nicotine", "Caffeine"
)

total_persons_all <- n_distinct(all_drugs_filtered$who)

illicit_low_prev <- all_drugs_filtered |>
  distinct(who, what_grouped) |>
  group_by(what_grouped) |>
  summarize(n_users = n(), .groups = "drop") |>
  mutate(pct_users = n_users / total_persons_all * 100) |>
  filter(pct_users < 1,
         !as.character(what_grouped) %in% prescribed_or_legal) |>
  pull(what_grouped)

all_drugs_filtered <- all_drugs_filtered |>
  filter(!what_grouped %in% illicit_low_prev)

all_drugs_filtered |>
  group_by(what_grouped) |>
  count() |>
  arrange(desc(n)) |>
  print(n = 30)


# ── Claude Computation begins ─────────────────────────────────────────
# The following code evaluates whether the drug_map groupings make
# sense given actual event counts in the filtered window. Run this
# block and inspect the output before finalising all_drugs_filtered.

# Total events and unique persons per drug category
drug_event_counts <- all_drugs_filtered |>
  group_by(what_grouped) |>
  summarize(
    n_events          = n(),
    n_persons         = n_distinct(who),
    events_per_person = n_events / n_persons,
    .groups           = "drop"
  ) |>
  arrange(desc(n_events))

print(drug_event_counts, n = 30)

# Proportion of all participants who used each drug at all
total_persons <- n_distinct(all_drugs_filtered$who)

drug_prevalence <- all_drugs_filtered |>
  distinct(who, what_grouped) |>
  group_by(what_grouped) |>
  summarize(n_users = n(), .groups = "drop") |>
  mutate(pct_users = round(n_users / total_persons * 100, 1)) |>
  arrange(desc(n_users))

print(drug_prevalence, n = 30)

# Interpretation guide:
# - drug_event_counts drives drugs_to_keep (n_events >= 10 filter
#   above). drug_prevalence is a DIAGNOSTIC ONLY — it is not used
#   for filtering. Use pct_users to check whether the binary
#   feature for a given drug is near-zero variance (pct_users < 1%
#   means almost nobody used it; binary_yn will carry no signal).
# - n_events 10–99 : sparse; sufficient to survive the filter but
#                    consider merging only if clinically similar
#                    AND the category offers no unique predictive
#                    value
# - n_events >= 100: sufficient to stand alone as a feature

# From Claude: Looks good!
# The groupings are clinically defensible. No major re-groupings
# are recommended. Two flags that warrant action before finalising:
#
# 1. WARNING — "Acetaminophen" in the "Opioid" group.
#    Acetaminophen (APAP) is NOT an opioid. It is likely appearing
#    here as the non-opioid component of combination pills
#    (Vicodin = hydrocodone + APAP; Percocet = oxycodone + APAP),
#    where both ingredients are logged as separate rows. If that
#    is the case, grouping APAP into "Opioid" double-counts those
#    use events (opioid_days gets inflated by APAP rows that are
#    already covered by the hydrocodone/oxycodone rows). Verify
#    with:
#      all_drugs |>
#        filter(what == "Acetaminophen") |>
#        left_join(
#          all_drugs |> filter(what != "Acetaminophen") |>
#            select(who, when) |> mutate(has_opioid = TRUE),
#          by = c("who", "when")
#        ) |>
#        count(has_opioid)
#    If most APAP rows DO have a co-occurring opioid on the same
#    day, remove Acetaminophen from drug_map entirely (it adds no
#    information the other opioid rows don't already carry).
#
# 2. CONSIDER — "Gabapentin" labelled "Analgesic".
#    In OUD populations gabapentin is a drug of misuse, not just a
#    prescribed pain medication. It potentiates opioid euphoria and
#    is used illicitly to manage withdrawal. Labelling it
#    "Analgesic" obscures this and groups it with benign
#    prescription use. Recommended: remove "Gabapentin" = "Analgesic"
#    from drug_map so it keeps its own name and stands alone as a
#    distinct category.
# ── Claude Computation ends ───────────────────────────────────────────


# ── Historical / commented-out exploration ────────────────────────────
# Preserved from exploratory_analysis.R for reference. Not intended
# to be run in this form.

# # Investigating the relationship between TLFB and all_drugs
# #
# # all_drugs has 248,428 "TFB" source rows — more than the 237,778
# # rows in the standalone tlfb table. The reason: participants can
# # use more than one drug per day, producing multiple rows per
# # patient-day in all_drugs but only one row per patient-day in tlfb.
# #
# # 1. Unique patient-days in the original TLFB table
# tlfb_days <- tlfb |>
#   distinct(who, when) |>
#   nrow()
#
# # 2. Unique patient-days for TFB sources inside all_drugs
# all_drugs_tfb_days <- all_drugs |>
#   filter(source == "TFB") |>
#   distinct(who, when) |>
#   nrow()
#
# print(paste("Original TLFB Unique Days:", tlfb_days))
# print(paste("All_Drugs TFB Unique Days:", all_drugs_tfb_days))
#
# # Note from tlfb docs: records where people self-reported the
# # study drug *after* it was prescribed have been removed from
# # tlfb (but not from all_drugs).
#
# # Counts of all drugs by source (TFB only)
# all_drugs |> filter(source == "TFB") |> count(what) |> print(n = 54)
# # Study drugs present: Buprenorphine, Methadone, Suboxone
#
# # Remove active-trial study drug records from all_drugs TFB rows
# all_drugs_tfb_consistent <- all_drugs |>
#   filter(
#     !(source == "TFB" &
#       when >= 0 &
#       what %in% c("Buprenorphine", "Suboxone", "Methadone"))
#   )
#
# # Compare drug categories: tlfb vs all_drugs (pre-study only)
# tlfb |>
#   filter(when < 0) |>
#   count(what) |>
#   print(n = 34)
#
# all_drugs |>
#   filter(when < 0, source == "TFB") |>
#   count(what) |>
#   print(n = 54)
#
# # Conclusion: all_drugs is more granular. Using all_drugs_filtered
# # with manual drug_map groupings gives better control than collapsing
# # the tlfb categories.
