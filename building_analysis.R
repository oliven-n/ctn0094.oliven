# ════════════════════════════════════════════════════════════════════
# building_analysis.R
#
# Builds the analysis tibble: one row per participant, one column
# per feature. Follows Features_To_Include_Accepted_Suggestions.Rmd
# (sections in order) and drug_specific_features.md (per-drug
# decisions). No outcome variable — relapse labels will be joined
# separately at modelling time.
#
# Window for all longitudinal data: days -28 (inclusive) to 0
# (exclusive). Gives exactly 4 of each weekday per person.
# ════════════════════════════════════════════════════════════════════


# ── 0. Libraries ──────────────────────────────────────────────────

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

first_rand <- randomization |>
  group_by(who) |>
  slice_min(when, n = 1, with_ties = FALSE) |>
  ungroup()

# INSPECT — run this to confirm column names before proceeding
glimpse(randomization)
# Expected: who, when (day 0 = randomization), and a treatment column.
# TODO: replace <TREATMENT_COL> below with the actual treatment column name.

analysis_base <- everybody |>
  inner_join(first_rand, by = "who") |>
  select(who, project)


# ── 2. all_drugs setup ────────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → all_drugs]

drug_map <- c(
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

# Primary filter: window + >= 10 events per category
drugs_to_keep <- all_drugs_grouped |>
  filter(when < 0, when >= -28) |>
  count(what_grouped) |>
  filter(n >= 10) |>
  pull(what_grouped)

all_drugs_filtered <- all_drugs_grouped |>
  filter(when < 0, when >= -28) |>
  filter(what_grouped %in% drugs_to_keep)

# Secondary filter: drop illicit categories with < 1% user prevalence
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

# Confirm surviving categories
surviving_drugs <- as.character(unique(all_drugs_filtered$what_grouped))
cat("Surviving drug categories:\n")
print(sort(surviving_drugs))


# ── 3. Nicotine check ─────────────────────────────────────────────
# [drug_specific_features.md → Nicotine]
#
# Nicotine was not visible in the pre-study events table. Investigate
# before deciding whether to include nicotine features.

nicotine_check <- all_drugs |>
  filter(grepl("nic|tobacco|smok|cig", what, ignore.case = TRUE)) |>
  count(what)
print(nicotine_check)
# If output is empty, nicotine is not in all_drugs. Rely on Fagerstrom only.
# If records exist, check whether "Nicotine" (or variant) survived the
# all_drugs_filtered pipeline and add it to drug_map if needed.

nicotine_present <- "Nicotine" %in% surviving_drugs


# ── 4. Helper functions ───────────────────────────────────────────

# Longest consecutive-day streak for a sorted integer vector of days.
# Returns 0 for empty input, 1 for isolated days with no consecutive runs.
longest_streak <- function(days) {
  if (length(days) == 0L) return(0L)
  days <- sort(unique(as.integer(days)))
  if (length(days) == 1L) return(1L)
  d <- diff(days)
  r <- rle(d)
  consec <- r$lengths[r$values == 1L]
  if (length(consec) == 0L) return(1L)
  max(consec) + 1L
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

  # Hard / Illicit Drugs
  drug_feature_block("Heroin",          "heroin"),
  drug_feature_block("Fentanyl",        "fentanyl"),
  drug_feature_block("Cocaine",         "cocaine"),
  drug_feature_block("Crack",           "crack"),
  drug_feature_block("Methamphetamine", "methamphetamine"),
  drug_feature_block("Amphetamine",     "amphetamine"),     # new; 844 events

  # Sedatives / CNS Depressants
  drug_feature_block("Benzodiazepine",  "benzodiazepine"),
  drug_feature_block("Sedatives",       "sedatives"),       # 263 events

  # Cannabis
  drug_feature_block("Cannabinoids",    "cannabinoids"),    # THC + K2

  # Alcohol
  drug_feature_block("Heavy Drinking",       "alcohol_hard"),
  drug_feature_block("Light Drinking",       "alcohol_light",       streak = FALSE),
  drug_feature_block("Alcohol Missing Amnt", "alcohol_missing_amnt", streak = FALSE),

  # Study Medications (pre-randomisation)
  drug_feature_block("Buprenorphine",   "buprenorphine",   streak = FALSE),
  drug_feature_block("Suboxone",        "suboxone",        streak = FALSE),
  drug_feature_block("Methadone",       "methadone",       streak = FALSE),

  # Prescription Opioids
  drug_feature_block("Opioid",          "opioid"),

  # Prescription / Non-Addictive Medications (standalone features)
  drug_feature_block("Analgesic",       "analgesic",       streak = FALSE),  # 23 events
  drug_feature_block("Antiemetic",      "antiemetic",      streak = FALSE),  # 16 events; also feeds rx composite

  # Conditional: Antidepressant — only include if it survived the filter
  if ("Antidepressant" %in% surviving_drugs)
    drug_feature_block("Antidepressant", "antidepressant", streak = FALSE)
  else
    NULL,

  # Conditional: Nicotine — only include if records exist in all_drugs
  if (nicotine_present)
    drug_feature_block("Nicotine", "nicotine", streak = FALSE)
  else
    NULL,

  # FLAGGED FOR DELETION: MDMA/Hallucinogen
  # ~100 events after merge; likely < 1% prevalence; weak OUD link.
  # Remove this block once secondary filter confirms it is dropped,
  # or at next pipeline cleanup.
  if ("MDMA/Hallucinogen" %in% surviving_drugs)
    drug_feature_block("MDMA/Hallucinogen", "mdma", streak = FALSE)
  else
    NULL

) |> purrr::compact()   # drop NULLs from conditional blocks

# Merge all drug feature tibbles into one wide tibble
drug_feats <- reduce(drug_feat_list, full_join, by = "who")


# ── 6. Derived alcohol feature ────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → all_drugs]
#
# "Restraint" = heavy drinking days - light drinking days.
# Computed after joining raw counts; requires both columns to exist.

drug_feats <- drug_feats |>
  mutate(
    alcohol_restraint = replace_na(alcohol_hard_days, 0L) -
                        replace_na(alcohol_light_days, 0L)
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

uds_positive_days <- all_drugs |>
  filter(source %in% c("UDS", "UDSAB"), when >= -28, when < 0) |>
  distinct(who, when)

tfb_positive_days <- all_drugs |>
  filter(source == "TFB", when >= -28, when < 0) |>
  distinct(who, when)

# Days where UDS/UDSAB found something but TLFB reported nothing
lie_days <- anti_join(uds_positive_days, tfb_positive_days, by = c("who", "when"))

lie_count <- lie_days |>
  group_by(who) |>
  summarize(lie_count = n(), .groups = "drop")

uds_total_days <- uds_positive_days |>
  group_by(who) |>
  summarize(uds_positive_total = n(), .groups = "drop")

lie_feats <- lie_count |>
  left_join(uds_total_days, by = "who") |>
  mutate(
    lie_rate = lie_count / uds_positive_total  # NA if uds_positive_total == 0
  ) |>
  select(who, lie_count, lie_rate)


# ── 9. ASI features ───────────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → asi]

# INSPECT — run this and check column names before proceeding
glimpse(asi)
# Expected: who + iv_drug_use (or similar) + ASI composite subscores
# (e.g. asi_drug, asi_alcohol, asi_legal, asi_family, asi_psychiatric,
#  asi_employment, asi_medical — exact names depend on dataset version)
# TODO: replace <IV_COL> and <COMPOSITE_COLS> below with actual names

asi_feats <- asi |>
  group_by(who) |>
  slice(1) |>   # one row per person (ASI is baseline, should already be unique)
  ungroup() |>
  transmute(
    who,
    # TODO: replace with actual IV drug use column name
    # asi_iv_binary = as.integer(<IV_COL> == "Yes"),

    # TODO: include ASI composite subscores if present, e.g.:
    # asi_drug_composite       = <drug_composite_col>,
    # asi_alcohol_composite    = <alcohol_composite_col>,
    # asi_legal_composite      = <legal_composite_col>,
    # asi_family_composite     = <family_composite_col>,
    # asi_psychiatric_composite = <psychiatric_composite_col>,
    # asi_employment_composite  = <employment_composite_col>,
    # asi_medical_composite     = <medical_composite_col>
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

# INSPECT — run this and check column names
glimpse(fagerstrom)
# Expected: who + item columns summing to a total FTND score (0-10)
# TODO: replace with actual column name(s)

fager_feats <- fagerstrom |>
  group_by(who) |>
  slice(1) |>
  ungroup() |>
  transmute(
    who
    # TODO: fagerstrom_score = <total_score_col>
    # If only items are present: fagerstrom_score = <item1> + <item2> + ...
  )


# ── 12. Pain: main effect ─────────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → pain]

# INSPECT — run this and check column names
glimpse(pain)
# Expected: who, when, and a pain score column (numeric, e.g. 0-10 NRS)
# TODO: replace <PAIN_COL> with the actual column name throughout this section

pain_window <- pain |>
  filter(when >= -28, when < 0)
  # TODO: select(who, when, pain_score = <PAIN_COL>)

pain_main_feats <- pain_window |>
  group_by(who) |>
  summarize(
    pain_mean = mean(pain_score, na.rm = TRUE),
    pain_max  = max(pain_score,  na.rm = TRUE),
    .groups   = "drop"
  )


# ── 13. Psychiatric features ──────────────────────────────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → psychiatric]

# INSPECT — run this and check column names
glimpse(psychiatric)
# Expected: who + diagnosis/flag columns for depression, anxiety, PTSD,
# and possibly others. Likely binary (0/1) or factor columns.
# TODO: replace column names below

psych_feats <- psychiatric |>
  group_by(who) |>
  slice(1) |>
  ungroup() |>
  transmute(
    who
    # TODO:
    # depression_binary    = as.integer(<depression_col>),
    # anxiety_binary       = as.integer(<anxiety_col>),
    # ptsd_binary          = as.integer(<ptsd_col>),
    # psych_comorbidity_n  = depression_binary + anxiety_binary + ptsd_binary
    #                        # add more diagnosis columns as needed
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
  slice_min(when, n = 1, with_ties = FALSE) |>  # baseline only
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

# TODO: replace <TREATMENT_COL> with actual column name from randomization
# The treatment value strings below are guesses — adjust to match actual levels.
study_drug_feats <- first_rand |>
  select(who, treatment = everything(), -who) |>   # TODO: replace with select(who, treatment = <TREATMENT_COL>)
  left_join(pre_moud_use, by = "who") |>
  mutate(
    # Map treatment to the drug group(s) that count as "their drug"
    # TODO: adjust the grepl patterns to match actual treatment level strings
    took_their_study_drug = case_when(
      grepl("buprenorphine|suboxone|naloxone", treatment, ignore.case = TRUE) &
        as.character(what_grouped) %in% c("Buprenorphine", "Suboxone") ~ 1L,
      grepl("methadone", treatment, ignore.case = TRUE) &
        as.character(what_grouped) == "Methadone" ~ 1L,
      TRUE ~ 0L
    ),
    took_different_study_drug = case_when(
      grepl("buprenorphine|suboxone|naloxone", treatment, ignore.case = TRUE) &
        as.character(what_grouped) == "Methadone" ~ 1L,
      grepl("methadone", treatment, ignore.case = TRUE) &
        as.character(what_grouped) %in% c("Buprenorphine", "Suboxone") ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  group_by(who) |>
  summarize(
    took_their_study_drug     = max(took_their_study_drug,     na.rm = TRUE),
    took_different_study_drug = max(took_different_study_drug, na.rm = TRUE),
    .groups = "drop"
  )


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
  summarize(
    pain_harddrug_corr = {
      nd <- sum(harddrug_use)
      if (nd == 0L) 0
      else if (nd == n()) NA_real_
      else suppressWarnings(cor(pain_score, harddrug_use, use = "complete.obs"))
    },
    pain_highrisk_harddrug_rate = {
      med <- median(pain_score, na.rm = TRUE)
      high_pain_days <- pain_score > med
      if (sum(high_pain_days, na.rm = TRUE) == 0L) NA_real_
      else mean(harddrug_use[high_pain_days], na.rm = TRUE)
    },
    .groups = "drop"
  )


# ── 22. Database Notes: Pain × soft drug interaction ─────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → Database Notes: pain x tlfb 2]
#
# Soft drug composite: Cannabinoids, Light Drinking, Caffeine
# (Alcohol hard excluded; Nicotine optional)
#
# Feature 1 — pain_softdrug_corr (expect negative or near zero)
# Feature 2 — pain_highrisk_softdrug_rate (expect negative association with relapse)

soft_drug_cats <- c("Cannabinoids", "Light Drinking", "Caffeine")

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
  summarize(
    pain_softdrug_corr = {
      nd <- sum(softdrug_use)
      if (nd == 0L) 0
      else if (nd == n()) NA_real_
      else suppressWarnings(cor(pain_score, softdrug_use, use = "complete.obs"))
    },
    pain_highrisk_softdrug_rate = {
      med <- median(pain_score, na.rm = TRUE)
      high_pain_days <- pain_score > med
      if (sum(high_pain_days, na.rm = TRUE) == 0L) NA_real_
      else mean(softdrug_use[high_pain_days], na.rm = TRUE)
    },
    .groups = "drop"
  )


# ── 23. Database Notes: Withdrawal trajectory ─────────────────────
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
  summarize(
    withdrawal_slope = if (n() >= 2L) {
      coef(lm(withdrawal_score ~ when, data = cur_data()))[["when"]]
      # TODO: replace withdrawal_score with actual column name
    } else {
      NA_real_
    },
    .groups = "drop"
  )


# ── 24. Database Notes: Medication adherence composite ────────────
# [Features_To_Include_Accepted_Suggestions.Rmd → Database Notes]
#
# ┌─────────────────────────────────────────────────────────────────┐
# │ NOTE: Review rx composite assembly carefully.                   │
# │                                                                 │
# │ Categories included: Antidepressant, Analgesic,                │
# │   Muscle Relaxant, Antiemetic                                   │
# │                                                                 │
# │ Benzodiazepine and Sedatives are prescribed but addictive;     │
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


# ── 25. Final assembly ────────────────────────────────────────────
# Left-join all feature tibbles onto the base (randomised participants only).
# NAs for drug count/binary/streak features are filled with 0 (= no use).
# NAs for clinical and demographic features are left as NA (genuine missing).

feature_list <- list(
  drug_feats,
  polydrug_days,
  drug_breadth,
  lie_feats,
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
  withdrawal_traj_feats,
  rx_feats
)

analysis_tibble <- reduce(
  feature_list,
  ~left_join(.x, .y, by = "who"),
  .init = analysis_base
) |>
  # Fill NAs with 0 for all drug count / streak / binary columns.
  # These NAs mean "did not use this substance" = 0, not "missing data".
  mutate(across(
    matches("_days$|_streak$|_binary$|polydrug_days|drug_breadth|lie_count|rx_days|rx_categories|rx_any_binary"),
    ~replace_na(.x, 0)
  )) |>
  # lie_rate: 0/0 is NA — leave as NA rather than imputing 0
  # (a person with no UDS records cannot have a lie rate computed)
  mutate(
    alcohol_restraint = replace_na(alcohol_restraint, 0)
  )

glimpse(analysis_tibble)
cat("\nDimensions:", nrow(analysis_tibble), "rows x", ncol(analysis_tibble), "cols\n")
