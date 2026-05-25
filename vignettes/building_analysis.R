# ════════════════════════════════════════════════════════════════════
# vignettes/building_analysis.R
#
# Builds the analysis tibble: one row per participant, one column
# per feature. Follows Features_To_Include_Accepted_Suggestions.Rmd
# (sections in order) and drug_specific_features.md (per-drug
# decisions). No outcome variable — relapse labels will be joined
# separately at modelling time.
#
# Window for all longitudinal data: days -28 (inclusive) to 0
# (exclusive). Gives exactly 4 of each weekday per person.
#
# SOURCED BY: vignettes/analysis.qmd
# Sections marked TODO return a who-only tibble until column names
# are resolved interactively via the glimpse() calls.
# ════════════════════════════════════════════════════════════════════


# ── 0. Libraries ──────────────────────────────────────────────────
library(tidyverse) #added in 5-19, block 2 was throwing errors with mutate() and .default?
library(dplyr) #redundant with above? also added in 5-19 after updating dplyr
library(conflicted)
suppressPackageStartupMessages(library(tidyverse))
library(public.ctn0094data)
conflicts_prefer(dplyr::filter)




# ── 1. Base tibble: first randomization only ──────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → randomization]
#
# inner_join with randomization ensures only randomised participants
# are included. slice_min(when) picks the earliest randomization
# per person, excluding the CTN-0030 re-randomization step.

# rewrote the first rand function to be more concise, checked identical
first_rand <- randomization |> filter(which==1)
glimpse(first_rand)


# INSPECT — run this to confirm column names before proceeding
glimpse(randomization)
# Expected: who, when (day 0 = randomization), and a treatment column.
# TODO: replace <TREATMENT_COL> below with the actual treatment column name.
# N: not seeing placeholder here. treatment col is "project"

#probably should rename this everybody_one_rand because thats what it is (NOT YET)
analysis_base <- everybody |>
  inner_join(first_rand, by = "who") |>
  select(who, project)

glimpse(analysis_base)



# ── 2. all_drugs setup ────────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → all_drugs]

drug_map <- c(
  "Acetaminophen"          = "Analgesic",
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
  "Sedative-Hypnotic"      = "Sedatives",
  "Barbiturate"            = "Sedatives",
  "Trazodone"              = "Antidepressant",
  "Tryclic-Antidepressant" = "Antidepressant",
  "Gabapentin"             = "Analgesic",
  "Thc"                    = "Cannabinoids",
  "K2"                     = "Cannabinoids",
  "Musclerelax"            = "Muscle Relaxant",
  "Mdma"                   = "MDMA/Hallucinogen",
  "Hallucinogen"           = "MDMA/Hallucinogen",
  # Not aggregating — renaming only so all alcohol categories sort together alphabetically
  "Heavy Drinking"         = "Alcohol Heavy Amnt",
  "Light Drinking"         = "Alcohol Light Amnt",
  "Alcohol"                = "Alcohol Missing Amnt"
)

# all_drugs_grouped_old <- all_drugs |>
#   mutate(
#     what_grouped = ifelse(
#       what %in% names(drug_map),
#       drug_map[as.character(what)],
#       as.character(what)
#     ),
#     what_grouped = factor(what_grouped)
#   )

# Mutates the 'what' in all_drugs by the table's rule, makes new what_grouped col
# like the above, but keeps metadata on the original labels.
all_drugs_grouped <- all_drugs |>
  mutate(
    what_grouped = case_when(
      what %in% names(drug_map) ~ drug_map[as.character(what)],
      .default = as.character(what)
    ),
    what_grouped = factor(what_grouped)
  )
glimpse(all_drugs_grouped)
# you can run the line 'names(all_drugs_grouped$what_grouped)' and get metadata


# identical(all_drugs_grouped_old, all_drugs_grouped)
# # gives FALSE because case_when keeps OG labels as metadata
# isTRUE(all.equal(all_drugs_grouped_old, all_drugs_grouped, check.attributes = FALSE))
# # Returns: TRUE because the above just compares elements


# Primary filter: window + >= 10 events per category
drugs_to_keep <- all_drugs_grouped |>
  filter(when < 0, when >= -28) |>
  count(what_grouped) |>
  filter(n >= 10) |>
  pull(what_grouped) |>
  unname()

all_drugs_filtered <- all_drugs_grouped |>
  filter(when < 0, when >= -28) |>
  filter(what_grouped %in% drugs_to_keep)

# Secondary filter: remove <1% users. NOT in effect yet.

# (Claude's implementation — NOT in pipeline, commented out):
# I didn't like it's weird manual exception system, it was arbitrary

# prescribed_or_legal <- c(
#   "Buprenorphine", "Suboxone", "Methadone",
#   "Benzodiazepine", "Sedatives", "Opioid",
#   "Muscle Relaxant", "Analgesic", "Antidepressant", "Antiemetic",
#   "Alcohol Heavy Amnt", "Alcohol Light Amnt", "Alcohol Missing Amnt",
#   "Cannabinoids", "Nicotine", "Caffeine"
# )
#
# total_persons_all <- n_distinct(all_drugs_filtered$who)
# illicit_low_prev <- all_drugs_filtered |>
#   distinct(who, what_grouped) |>
#   group_by(what_grouped) |>
#   summarize(n_users = n(), .groups = "drop") |>
#   mutate(pct_users = n_users / total_persons_all * 100) |>
#   filter(pct_users < 1,
#          !as.character(what_grouped) %in% prescribed_or_legal) |>
#   pull(what_grouped)


# ----- Begin Nat's Edits
# Nat's implementation of secondary crit, still not in effect

total_persons_all <- n_distinct(all_drugs_filtered$who)
prevalence_table <- all_drugs_filtered |>
  distinct(who, what_grouped) |>
  group_by(what_grouped) |>
  summarize(n_users = n(), .groups = "drop") |>
  mutate(pct_users = n_users / total_persons_all * 100)

# Identifying the drugs that would be cut to see if sensible grouping(s) exist,
# with particular thought into what drugs are good indicators of medication adherence
# for making a composite signal of them later

#This is a cuter substitute for illicit_low_prev_nat, and identical() check gives true
low_prev_drugs <- prevalence_table |> select(what_grouped, pct_users) |>
  filter(pct_users < 1) |> pull(what_grouped) |> unname()

# Commenting the below line out because we are not updating all_drugs_filtered based on secondary crit
# all_drugs_filtered <- all_drugs_filtered |>
#   filter(!what_grouped %in% low_prev_drugs)


# ----- End Nat's Edits


# Confirm surviving categories
surviving_drugs <- as.character(unique(all_drugs_filtered$what_grouped))
cat("Surviving drug categories:\n")
print(sort(surviving_drugs))


# ── 3. (Reserved) ────────────────────────────────────────────────
# Nicotine is not present in all_drugs. Nicotine dependence is captured
# via the Fagerstrom score only (see later sections).


# ── 4. Helper functions ───────────────────────────────────────────

# Given a vector of day numbers, returns the length of the longest
# run of back-to-back consecutive days.
# Examples:  c(-5, -4, -3, -1)  →  3  (days -5 through -3)
#            c(-5, -3, -1)      →  1  (no two days are adjacent)
#            c()                →  0  (no days recorded)
longest_streak <- function(days) {

  # No days recorded at all
  if (length(days) == 0L) return(0L)

  # Remove duplicates and put days in order
  days <- sort(unique(as.integer(days)))

  # Only one day recorded — a streak of one
  if (length(days) == 1L) return(1L)

  # Calculate the gap between each adjacent pair of days.
  # A gap of exactly 1 means those two days are back-to-back.
  gaps <- diff(days)

  # Walk through the gaps, counting the current streak and
  # keeping track of the longest one seen so far.
  current_streak <- 1L
  longest        <- 1L

  for (gap in gaps) {
    if (gap == 1L) {
      # Days are consecutive — extend the current streak
      current_streak <- current_streak + 1L
    } else {
      # A gap in days — start a new streak from scratch
      current_streak <- 1L
    }

    # Update the record if the current streak is the longest so far
    if (current_streak > longest) longest <- current_streak
  }

  return(longest)
}

# Compute _days, _streak (optional), _binary for one drug category.
# Returns a tibble with columns: who, <prefix>_days, [<prefix>_streak], <prefix>_binary
drug_feature_block <- function(drug_name, prefix, streak = TRUE,
                                data = all_drugs_filtered) {
  d <- data |>
    filter(as.character(what_grouped) == drug_name) |>
    distinct(who, when) |>
    arrange(who, when)

  days_tbl <- d |>
    group_by(who) |>
    summarize(!!paste0(prefix, "_days") := n(), .groups = "drop")

  bin_tbl <- d |>
    distinct(who) |>
    mutate(!!paste0(prefix, "_binary") := 1L)

  out <- left_join(days_tbl, bin_tbl, by = "who")

  if (streak) {
    streak_tbl <- d |>
      group_by(who) |>
      summarize(!!paste0(prefix, "_streak") := longest_streak(when),
                .groups = "drop")
    out <- left_join(out, streak_tbl, by = "who")
  }
  out
}


# ── 5. Per-drug features ──────────────────────────────────────────
# [drug_specific_features.md]
#
# Each call to drug_feature_block() maps to one row in the
# drug_specific_features.md summary table.

drug_feat_list <- list(

  # 21 calls = 21 surviving categories after primary filter (>= 10 events).
  # Ordered alphabetically by drug name. Prefixes match drug names exactly
  # (underscores for spaces, case-insensitive).

  drug_feature_block("Alcohol Heavy Amnt",   "alcohol_heavy_amnt"),
  # TODO (for you): revisit alcohol_light_amnt streak — omitted because its
  # 1,704 events are in the same ballpark as heavy (1,719); light drinking
  # streaks also have low clinical significance for OUD prediction.
  drug_feature_block("Alcohol Light Amnt",   "alcohol_light_amnt",   streak = FALSE),
  # TODO (for you): revisit alcohol_missing_amnt streak — omitted because its
  # 1,449 events are in the same ballpark as heavy (1,719) and light (1,704).
  # Re-check if usage patterns closely track heavy drinking before re-including.
  drug_feature_block("Alcohol Missing Amnt", "alcohol_missing_amnt", streak = FALSE),
  drug_feature_block("Amphetamine",          "amphetamine"),
  drug_feature_block("Analgesic",            "analgesic",            streak = FALSE),
  drug_feature_block("Antiemetic",           "antiemetic",           streak = FALSE),
  drug_feature_block("Benzodiazepine",       "benzodiazepine"),
  drug_feature_block("Buprenorphine",        "buprenorphine",        streak = FALSE),
  drug_feature_block("Cannabinoids",         "cannabinoids"),
  drug_feature_block("Cocaine",              "cocaine"),
  drug_feature_block("Crack",               "crack"),
  drug_feature_block("Fentanyl",             "fentanyl"),
  drug_feature_block("Heroin",              "heroin"),
  drug_feature_block("MDMA/Hallucinogen",    "mdma_hallucinogen",    streak = FALSE),
  drug_feature_block("Methadone",            "methadone",            streak = FALSE),
  drug_feature_block("Methamphetamine",      "methamphetamine"),
  drug_feature_block("Muscle Relaxant",      "muscle_relaxant",      streak = FALSE),
  drug_feature_block("Opioid",              "opioid"),
  drug_feature_block("PCP",                 "pcp"),
  drug_feature_block("Sedatives",            "sedatives"),
  drug_feature_block("Suboxone",             "suboxone",             streak = FALSE)

) |> purrr::compact()

# Merge all drug feature tibbles into one wide tibble
drug_feats <- reduce(drug_feat_list, full_join, by = "who")


# ── 6. Derived alcohol feature ────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → all_drugs]
#
# "Restraint" = light drinking days - heavy drinking days.
# Computed after joining raw counts; requires both columns to exist.

drug_feats <- drug_feats |>
  mutate(
    alcohol_restraint = replace_na(alcohol_light_amnt_days, 0L) -
                        replace_na(alcohol_heavy_amnt_days, 0L)
  )


# ── 7. Polydrug & breadth features ───────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → all_drugs Dataset Notes]

# Days on which >= 2 distinct drug categories were used simultaneously
polydrug_days <- all_drugs_filtered |>
  distinct(who, when, what_grouped) |>
  group_by(who, when) |>
  summarize(n_cats = n_distinct(what_grouped), .groups = "drop") |>
  filter(n_cats >= 2) |>
  group_by(who) |>
  summarize(polydrug_days = n(), .groups = "drop")

# Number of distinct drug categories used at all in the window
drug_breadth <- all_drugs_filtered |>
  distinct(who, what_grouped) |>
  group_by(who) |>
  summarize(drug_breadth = n_distinct(what_grouped), .groups = "drop")


# ── 8. Lie count ─────────────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → all_drugs]
#
# Definition (Option A — day-level, not substance-level):
#   A "lie day" is any day in the window where UDS or UDSAB recorded
#   a positive result but the participant reported nothing in TLFB.
#   Over-reporting (TLFB positive, UDS negative) does NOT count.
#
# NOTE: Option A is a crude proxy. It compares any-drug presence
# across sources, not substance-matched positivity. A UDS positive
# for cocaine on a day the participant only reported heroin would
# count as a lie even if the heroin report was honest.
# Consider upgrading to substance-level matching later.

# DROPPED — lie_count / lie_rate
# For nearly all participants there were no UDS or UDSAB screenings in the
# pre-trial window. For those with screenings, self-report was more or less
# consistent with UDS results, up to drug category grouping name differences.
# This resulted in lie_count and lie_rate being NA or 0 for the vast majority
# of participants. Features carry no predictive signal; excluded from tibble.
#
# uds_positive_days <- all_drugs |>
#   filter(source %in% c("UDS", "UDSAB"), when >= -28, when < 0) |>
#   distinct(who, when)
#
# tfb_positive_days <- all_drugs |>
#   filter(source == "TFB", when >= -28, when < 0) |>
#   distinct(who, when)
#
# # Days where UDS/UDSAB found something but TLFB reported nothing
# lie_days <- anti_join(uds_positive_days, tfb_positive_days, by = c("who", "when"))
#
# lie_count <- lie_days |>
#   group_by(who) |>
#   summarize(lie_count = n(), .groups = "drop")
#
# uds_total_days <- uds_positive_days |>
#   group_by(who) |>
#   summarize(uds_positive_total = n(), .groups = "drop")
#
# lie_feats <- lie_count |>
#   left_join(uds_total_days, by = "who") |>
#   mutate(
#     lie_rate = lie_count / uds_positive_total  # NA if uds_positive_total == 0
#   ) |>
#   select(who, lie_count, lie_rate)


# ── 9. ASI features ───────────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → asi]
#
# glimpse(asi): 2 columns — who (int), used_iv (fct: "Yes"/"No"/NA).
# No ASI composite subscores present in this dataset version.

asi_feats <- asi |>
  group_by(who) |>
  slice(1) |>   # one row per person (asi is already unique per who)
  ungroup() |>
  transmute(
    who,
    asi_iv_binary = as.integer(used_iv == "Yes")
    # NOTE: ASI composite subscores (drug, alcohol, legal, family, psychiatric,
    # employment, medical) are not present in this dataset version.
  )


# ── 10. Demographics features ─────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → demographics]
#
# All demographic variables are baseline (no temporal component).
# Categorical variables are kept as-is for now; downstream modelling
# code should dummy-encode as needed.

demo_feats <- demographics |>
  transmute(
    who,
    age,
    is_hispanic = as.integer(tolower(as.character(is_hispanic)) == "yes"),
    race,
    job,
    is_living_stable = as.integer(tolower(as.character(is_living_stable)) == "yes"),
    education,
    marital,
    is_married    = as.integer(tolower(as.character(marital)) == "married"),
    is_male       = as.integer(tolower(as.character(is_male)) == "yes")
  )


# ── 11. Fagerstrom ────────────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → fagerstrom]
#
# glimpse(fagerstrom): who (int), is_smoker (fct: Yes/No),
# ftnd (fct: 0–10, NA for non-smokers), per_day (fct: 10 OR LESS /
# 11-20 / 21-30 / 31 OR MORE / "" for non-smokers).
# Non-smoker NAs in ftnd imputed to "0"; "" in per_day relabelled "None".
# Both kept as ordered factors for flexibility at recipe time.

fager_feats <- fagerstrom |>
  group_by(who) |>
  slice(1) |>
  ungroup() |>
  transmute(
    who,
    # to binary on is_smoker
    is_smoker  = as.integer(tolower(as.character(is_smoker)) == "yes"),
    # ordered factor; non-smoker NAs imputed to "0" (unambiguously zero dependence)
    ftnd_score = ordered(
      if_else(is_smoker == 0, "0", as.character(ftnd)),
      levels = as.character(0:10)
    ),
    # "" means non-smoker; relabelled "None" and given explicit ordered levels
    per_day    = factor(
      if_else(as.character(per_day) == "", "None", as.character(per_day)),
      levels  = c("None", "10 OR LESS", "11-20", "21-30", "31 OR MORE"),
      ordered = TRUE
    )
  )


# ── 12. Pain: main effect ─────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → pain]
#
# glimpse(pain): who (int), when (int), pain (fct: "No Pain" /
# "Very mild to Moderate Pain" / "Severe Pain" / "Missing").
# Scored ordinally: No Pain → 0, Very mild to Moderate → 1, Severe → 2, Missing → NA.

pain_window <- pain |>
  filter(when >= -28, when < 0) |>
  transmute(
    who,
    when,
    pain_score = case_when(
      pain == "No Pain"                    ~ 0L,
      pain == "Very mild to Moderate Pain" ~ 1L,
      pain == "Severe Pain"                ~ 2L,
      .default = NA_integer_
    )
  )

pain_main_feats <- pain_window |>
  group_by(who) |>
  summarise(
    pain_mean         = mean(pain_score, na.rm = TRUE),
    pain_max          = max(pain_score,  na.rm = TRUE),
    pct_days_severe   = mean(pain_score == 2, na.rm = TRUE),
    pct_days_any_pain = mean(pain_score  > 0, na.rm = TRUE),
    .groups = "drop"
  )


# ── 13. Psychiatric features ──────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → psychiatric]
#
# glimpse(psychiatric): 16 cols — who (int); 6 medical-history diagnosis flags
# (has_schizophrenia / has_major_dep / has_bipolar / has_anx_pan /
# has_brain_damage / has_epilepsy): fct Yes/No/NA; 3 ASI self-report items
# (depression / anxiety / schizophrenia): fct Yes/No/Not answered/Missing/NA;
# has_opiates_dx + 5 substance-dx flags: fct Yes/No/NA.
#
# Simple Yes/No flags: as.integer(x == "Yes") propagates NA naturally.
# ASI items: "Not answered" and "Missing" → NA via case_when .default.
# has_opiates_dx excluded from substance_dx_count (near-universal in sample).

psych_feats <- psychiatric |>
  group_by(who) |>
  slice(1) |>
  ungroup() |>
  transmute(
    who,
    # Medical history diagnosis flags (Yes/No/NA)
    has_schizophrenia   = as.integer(has_schizophrenia   == "Yes"),
    has_major_dep       = as.integer(has_major_dep       == "Yes"),
    has_bipolar         = as.integer(has_bipolar         == "Yes"),
    has_anx_pan         = as.integer(has_anx_pan         == "Yes"),
    has_brain_damage    = as.integer(has_brain_damage    == "Yes"),
    has_epilepsy        = as.integer(has_epilepsy        == "Yes"),
    # DSM diagnosis flags (Yes/No/NA)
    has_opiates_dx      = as.integer(has_opiates_dx      == "Yes"),
    has_alcol_dx        = as.integer(has_alcol_dx        == "Yes"),
    has_amphetamines_dx = as.integer(has_amphetamines_dx == "Yes"),
    has_cannabis_dx     = as.integer(has_cannabis_dx     == "Yes"),
    has_cocaine_dx      = as.integer(has_cocaine_dx      == "Yes"),
    has_sedatives_dx    = as.integer(has_sedatives_dx    == "Yes"),
    # ASI self-report (Yes/No/Not answered/Missing → 1/0/NA/NA)
    depression_binary    = case_when(depression    == "Yes" ~ 1L, depression    == "No" ~ 0L, .default = NA_integer_),
    anxiety_binary       = case_when(anxiety       == "Yes" ~ 1L, anxiety       == "No" ~ 0L, .default = NA_integer_),
    schizophrenia_binary = case_when(schizophrenia == "Yes" ~ 1L, schizophrenia == "No" ~ 0L, .default = NA_integer_),
    # Derived: burden counts (NA-safe; NA inputs produce NA in sum, which is fine)
    psych_comorbidity_count = has_schizophrenia + has_major_dep + has_bipolar +
                              has_anx_pan + has_brain_damage + has_epilepsy,
    substance_dx_count      = has_alcol_dx + has_amphetamines_dx +
                              has_cannabis_dx + has_cocaine_dx + has_sedatives_dx,
    dep_or_anx_binary       = as.integer(depression_binary == 1 | anxiety_binary == 1)
  )


# ── 14. Quality of life ───────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → qol]

# INSPECT — run this and check column names
glimpse(qol)
# Expected: who + overall QOL score and/or physical/mental subscores
# (SF-36 or EQ-5D format). May have a when column if measured repeatedly.
# TODO: replace column names below

qol_feats <- qol |>
  group_by(who) |>
  slice(1) |>  # baseline only (slice_min(when) omitted until `when` column confirmed)
  ungroup() |>
  transmute(
    who
    # TODO:
    # qol_overall  = <overall_score_col>,
    # qol_physical = <physical_subscore_col>,
    # qol_mental   = <mental_subscore_col>
  )


# ── 15. Treatment arm ─────────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → randomization]

# INSPECT — confirm the treatment column name
glimpse(first_rand)
# TODO: replace <TREATMENT_COL> with the actual column name

rand_feats <- first_rand |>
  transmute(
    who
    # TODO: treatment_arm = <TREATMENT_COL>
  )


# ── 16. RBS features ─────────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → rbs]

# INSPECT — run this and check column names
glimpse(rbs)
# Expected: who + columns for social network drug use
# (e.g. proportion/count of network members who use drugs)
# and number of recent sexual partners
# TODO: replace column names below

rbs_feats <- rbs |>
  group_by(who) |>
  slice(1) |>
  ungroup() |>
  transmute(
    who
    # TODO:
    # social_network_drug_use = <network_drug_col>,
    # n_sexual_partners       = <partners_col>
  )


# ── 17. RBS_IV: recent IV drug use ───────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → rbs_iv]

# INSPECT — run this and check column names
glimpse(rbs_iv)
# Expected: who + binary or flag column for recent IV drug use
# TODO: replace column names below

rbs_iv_feats <- rbs_iv |>
  group_by(who) |>
  slice(1) |>
  ungroup() |>
  transmute(
    who
    # TODO: iv_drug_use_recent_binary = as.integer(<iv_col> == "Yes")
  )


# ── 18. Study site ────────────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → site_masked]

site_feats <- site_masked |>
  transmute(who, site = site_masked)  # TODO: confirm column name is site_masked


# ── 19. Withdrawal pre/post ───────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → withdrawal_pre_post]

# INSPECT — run this and check column names
glimpse(withdrawal_pre_post)
# Expected: who + pre-induction score + post-induction score
# (COWS or SOWS). May have a "pre" and "post" column or a "when" indicator.
# TODO: replace column names below

withdrawal_pp_feats <- withdrawal_pre_post |>
  group_by(who) |>
  slice(1) |>
  ungroup() |>
  transmute(
    who
    # TODO:
    # cows_pre   = <pre_score_col>,
    # cows_post  = <post_score_col>,
    # cows_delta = <post_score_col> - <pre_score_col>
  )


# ── 20. Database Notes: Took THEIR / DIFFERENT study drug ─────────
# [Features_To_Include_Accepted_Suggestions.Rmd → Database Notes]
#
# For each participant, check whether they used their assigned study
# drug (or its equivalent) in the pre-study window.
# Buprenorphine ≡ Suboxone for this purpose.
# XR-Naltrexone is not in all_drugs (not a drug of abuse) → always 0.

# INSPECT — confirm the treatment column name in first_rand
# glimpse(first_rand)  # already called above in section 15

# Pre-study MOUD use (any source)
pre_moud_use <- all_drugs_filtered |>
  filter(as.character(what_grouped) %in% c("Buprenorphine", "Suboxone", "Methadone")) |>
  distinct(who, what_grouped)

# TODO: replace <TREATMENT_COL> with the actual column name from randomization,
# then swap the placeholder below for the full implementation sketch (commented out).
study_drug_feats <- first_rand |>
  transmute(who)
  # Full implementation — uncomment after resolving treatment column name:
  #
  # study_drug_feats <- first_rand |>
  #   select(who, treatment = <TREATMENT_COL>) |>
  #   left_join(pre_moud_use, by = "who") |>
  #   mutate(
  #     took_their_study_drug = case_when(
  #       grepl("buprenorphine|suboxone|naloxone", treatment, ignore.case = TRUE) &
  #         as.character(what_grouped) %in% c("Buprenorphine", "Suboxone") ~ 1L,
  #       grepl("methadone", treatment, ignore.case = TRUE) &
  #         as.character(what_grouped) == "Methadone" ~ 1L,
  #       TRUE ~ 0L
  #     ),
  #     took_different_study_drug = case_when(
  #       grepl("buprenorphine|suboxone|naloxone", treatment, ignore.case = TRUE) &
  #         as.character(what_grouped) == "Methadone" ~ 1L,
  #       grepl("methadone", treatment, ignore.case = TRUE) &
  #         as.character(what_grouped) %in% c("Buprenorphine", "Suboxone") ~ 1L,
  #       TRUE ~ 0L
  #     )
  #   ) |>
  #   group_by(who) |>
  #   summarize(
  #     took_their_study_drug     = max(took_their_study_drug,     na.rm = TRUE),
  #     took_different_study_drug = max(took_different_study_drug, na.rm = TRUE),
  #     .groups = "drop"
  #   )


# ── 21. Database Notes: Pain × hard drug interaction ─────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → Database Notes: pain x tlfb 1]
#
# Hard drug composite: Heroin, Fentanyl, Cocaine, Crack,
#                      Methamphetamine, Amphetamine
#
# Feature 1 — pain_harddrug_corr:
#   Person-level Pearson r between daily pain score and daily hard-drug
#   binary (0/1) across days -28 to -1. Undefined correlation coded as 0
#   (0 hard drug days) or NA (all hard drug days — zero variance).
#
# Feature 2 — pain_highrisk_harddrug_rate:
#   Proportion of above-median pain days that also had hard drug use.

hard_drug_cats <- c("Heroin", "Fentanyl", "Cocaine", "Crack",
                    "Methamphetamine", "Amphetamine")

hard_drug_days_flag <- all_drugs_filtered |>
  filter(as.character(what_grouped) %in% hard_drug_cats) |>
  distinct(who, when) |>
  mutate(harddrug_use = 1L)

# TODO: replace pain_score with actual column name from pain dataset
pain_hard_daily <- pain |>
  filter(when >= -28, when < 0) |>
  # TODO: rename pain score column: select(who, when, pain_score = <PAIN_COL>)
  left_join(hard_drug_days_flag, by = c("who", "when")) |>
  mutate(harddrug_use = replace_na(harddrug_use, 0L))

pain_hard_feats <- pain_hard_daily |>
  group_by(who) |>
  summarize(.groups = "drop")
  # TODO: add after resolving pain_score column:
  # pain_harddrug_corr = {
  #   nd <- sum(harddrug_use)
  #   if (nd == 0L) 0
  #   else if (nd == n()) NA_real_
  #   else suppressWarnings(cor(pain_score, harddrug_use, use = "complete.obs"))
  # },
  # pain_highrisk_harddrug_rate = {
  #   med <- median(pain_score, na.rm = TRUE)
  #   high_pain_days <- pain_score > med
  #   if (sum(high_pain_days, na.rm = TRUE) == 0L) NA_real_
  #   else mean(harddrug_use[high_pain_days], na.rm = TRUE)
  # },


# ── 22. Database Notes: Pain × soft drug interaction ─────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → Database Notes: pain x tlfb 2]
#
# Soft drug composite: Cannabinoids, Alcohol Light Amnt, Caffeine
# (Alcohol Heavy Amnt excluded; Nicotine not in all_drugs; Caffeine dropped by primary filter)
#
# Feature 1 — pain_softdrug_corr (expect negative or near zero)
# Feature 2 — pain_highrisk_softdrug_rate (expect negative association with relapse)

soft_drug_cats <- c("Cannabinoids", "Alcohol Light Amnt", "Caffeine")

soft_drug_days_flag <- all_drugs_filtered |>
  filter(as.character(what_grouped) %in% soft_drug_cats) |>
  distinct(who, when) |>
  mutate(softdrug_use = 1L)

# TODO: replace pain_score with actual column name
pain_soft_daily <- pain |>
  filter(when >= -28, when < 0) |>
  # TODO: select(who, when, pain_score = <PAIN_COL>)
  left_join(soft_drug_days_flag, by = c("who", "when")) |>
  mutate(softdrug_use = replace_na(softdrug_use, 0L))

pain_soft_feats <- pain_soft_daily |>
  group_by(who) |>
  summarize(.groups = "drop")
  # TODO: add after resolving pain_score column:
  # pain_softdrug_corr = {
  #   nd <- sum(softdrug_use)
  #   if (nd == 0L) 0
  #   else if (nd == n()) NA_real_
  #   else suppressWarnings(cor(pain_score, softdrug_use, use = "complete.obs"))
  # },
  # pain_highrisk_softdrug_rate = {
  #   med <- median(pain_score, na.rm = TRUE)
  #   high_pain_days <- pain_score > med
  #   if (sum(high_pain_days, na.rm = TRUE) == 0L) NA_real_
  #   else mean(softdrug_use[high_pain_days], na.rm = TRUE)
  # },



# ── 23. Database Notes: Psychiatric × pain and psychiatric × drug ──
# [Features_To_Include_Accepted_Suggestions.Rmd → Database Notes]
#
# Interactions between psychiatric comorbidities (§13) and:
#   - pain main effects (§12): depression/anxiety amplify pain-triggered relapse
#   - benzodiazepine use (§5): anxiety + benzo co-use = elevated CNS/OD risk
# NA in benzodiazepine_days means no observed use → imputed 0 before multiplying.

psych_cross_feats <- analysis_base |>
  left_join(select(psych_feats,     who, has_major_dep, has_anx_pan), by = "who") |>
  left_join(select(pain_main_feats, who, pain_mean, pain_max),        by = "who") |>
  left_join(select(drug_feats,      who, benzodiazepine_days),        by = "who") |>
  transmute(
    who,
    has_major_dep_x_pain_mean         = has_major_dep * pain_mean,
    has_major_dep_x_pain_max          = has_major_dep * pain_max,
    has_anx_pan_x_pain_mean           = has_anx_pan   * pain_mean,
    has_anx_pan_x_pain_max            = has_anx_pan   * pain_max,
    has_anx_pan_x_benzodiazepine_days = has_anx_pan   * replace_na(benzodiazepine_days, 0L)
  )


# ── 24. Database Notes: Withdrawal trajectory ─────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → Database Notes]
#
# Slope of withdrawal symptom scores (COWS/SOWS) over days -28 to -1.
# Positive slope = worsening withdrawal = escalating dependence.
# Estimated per person via simple linear regression: score ~ when.

# INSPECT — run this to confirm column names
glimpse(withdrawal)
# Expected: who, when, and a score column (COWS or SOWS total)
# TODO: replace <SCORE_COL> with the actual column name

withdrawal_traj_feats <- withdrawal |>
  filter(when >= -28, when < 0) |>
  group_by(who) |>
  summarize(.groups = "drop")
  # TODO: add after resolving withdrawal score column:
  # withdrawal_slope = if (n() >= 2L) {
  #   coef(lm(<SCORE_COL> ~ when, data = cur_data()))[["when"]]
  # } else {
  #   NA_real_
  # },


# ── 25. Database Notes: Medication adherence composite ────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → Database Notes]
#
# ┌─────────────────────────────────────────────────────────────────┐
# │ NOTE: Review rx composite assembly carefully.                   │
# │                                                                 │
# │ Categories included: Analgesic, Muscle Relaxant, Antiemetic     │
# │ (Antidepressant dropped by primary filter — 3 events only)      │
# │                                                                 │
# │ Benzodiazepine and Sedatives are prescribed but addictive;      │
# │ they are NOT included here. Add them back if you want a         │
# │ broader "any Rx medication" feature rather than the             │
# │ "non-addictive Rx medication" construct.                        │
# │                                                                 │
# │ Antiemetic and Muscle Relaxant also appear as standalone        │
# │ features (antiemetic_days, antiemetic_binary) above.            │
# └─────────────────────────────────────────────────────────────────┘

rx_cats <- c("Antidepressant", "Analgesic", "Muscle Relaxant", "Antiemetic")
rx_cats_present <- intersect(rx_cats, surviving_drugs)

rx_days_data <- all_drugs_filtered |>
  filter(as.character(what_grouped) %in% rx_cats_present) |>
  distinct(who, when)

rx_feats <- rx_days_data |>
  group_by(who) |>
  summarize(rx_days = n(), .groups = "drop") |>
  left_join(
    all_drugs_filtered |>
      filter(as.character(what_grouped) %in% rx_cats_present) |>
      distinct(who, what_grouped) |>
      group_by(who) |>
      summarize(rx_categories = n_distinct(what_grouped), .groups = "drop"),
    by = "who"
  ) |>
  mutate(rx_any_binary = 1L)


# ── 26. Final assembly ────────────────────────────────────────────
# Left-join all feature tibbles onto the base (randomised participants only).
# NAs for drug count/binary/streak features are filled with 0 (= no use).
# NAs for clinical and demographic features are left as NA (genuine missing).

feature_list <- list(
  drug_feats,
  polydrug_days,
  drug_breadth,
  # lie_feats,  # DROPPED — see §8 note
  asi_feats,
  demo_feats,
  fager_feats,
  pain_main_feats,
  psych_feats,
  qol_feats,
  rand_feats,
  rbs_feats,
  rbs_iv_feats,
  site_feats,
  withdrawal_pp_feats,
  study_drug_feats,
  pain_hard_feats,
  pain_soft_feats,
  psych_cross_feats,
  withdrawal_traj_feats,
  rx_feats
)

analysis_tibble <- reduce(
  feature_list,
  ~left_join(.x, .y, by = "who"),
  .init = analysis_base
) |>
  # For drug features, NA means the person had zero observed events in the window —
  # not a data collection failure. No use = 0 days, 0 streak, 0 binary.
  # Clinical/demographic NAs are left as-is (genuine missing data).
  mutate(across(
    matches("_days$|_streak$|_binary$|polydrug_days|drug_breadth|rx_days|rx_categories|rx_any_binary"),
    ~replace_na(.x, 0)
  )) |>
  mutate(
    alcohol_restraint = replace_na(alcohol_restraint, 0)
  )

glimpse(analysis_tibble)
cat("\nDimensions:", nrow(analysis_tibble), "rows x", ncol(analysis_tibble), "cols\n")
