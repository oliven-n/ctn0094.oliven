# logistic_regression_without_problem_vars.R
#
# Structurally identical to logistic_regression.R but with:
#   1. All R objects suffixed _wopv so this file coexists in the same R session
#      as logistic_regression.R without overwriting its objects.
#   2. Known problem variables excluded from the recipe:
#        took_own_study_drug / took_other_study_drug  — leakage (encode treatment arm)
#        methadone_binary / buprenorphine_binary / suboxone_binary  — same leakage
#        muscle_relaxant_days  — separation–rareness (too few users)
#   3. per_day recipe bug fixed: per_day excluded from step_novel() and
#      step_unknown() so its 5-level polynomial basis is not over-expanded to 7
#      levels (which produced per_day_4/5/6 with NA GLM coefficients).
#
# Objects created (all suffixed _wopv):
#   lr_fit_wopv          — fitted logistic regression workflow
#   lr_metrics_wopv      — training-set metrics
#   lr_test_metrics_wopv — test-set metrics
#   lr_or_table_wopv     — OR table (p < 0.05, sorted by effect size)

# ── Standalone dev setup ─────────────────────────────────────────────────────
library(conflicted)
suppressPackageStartupMessages(library(tidymodels))
tidymodels_prefer()
suppressPackageStartupMessages(library(tidyverse))
library(public.ctn0094data)
library(CTNote)
library(gtsummary)
library(knitr)
conflicts_prefer(dplyr::filter)
options(dplyr.summarise.inform = FALSE)
source(here::here("vignettes/building_analysis.R"))

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !! NOTE — PROBLEM VARIABLES EXCLUDED FROM THIS MODEL
# !!
# !! took_own_study_drug / took_other_study_drug:
# !!   Drug-use data is pre-randomization, but the "own" / "other" labeling
# !!   requires knowing each participant's treatment arm assignment. Leakage.
# !!   → Excluded via step_rm() below.
# !!
# !! methadone_binary / buprenorphine_binary / suboxone_binary:
# !!   Pre-treatment medication-type flags proxy for treatment arm assignment.
# !!   → Excluded via step_rm() below.
# !!
# !! muscle_relaxant_days:
# !!   Used by too few participants → complete separation in training split.
# !!   → Excluded via step_rm() below.
# !!
# !! per_day polynomial fix (step_novel / step_unknown):
# !!   step_novel adds a "new" level and step_unknown adds an "unknown" level,
# !!   expanding per_day from 5 levels to 7 before step_dummy(one_hot = FALSE).
# !!   This generates 6 polynomial contrast columns for a 5-point factor —
# !!   two more than the data support — so the GLM drops per_day_4/5/6 as
# !!   rank-deficient. Fix: exclude per_day from both steps.
# !!
# !! withdrawal_pre_score: kept (pre-induction but post-randomization; flagged).
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# ── Outcome variable + Train-Test Split ──────────────────────────────────────
#
# select(-any_of("outcome")) guards against the case where logistic_regression.R
# was sourced first — that script mutates analysis_tibble in place, adding the
# outcome column. Without the guard a second left_join would produce outcome.x
# and outcome.y and crash the recipe. same seed → identical split → valid AUC comparison.
analysis_tibble_wopv <- analysis_tibble |>
  select(-any_of("outcome")) |>
  left_join(
    outcomesCTN0094 |> select(who, outcome = lee2018_rel_event),
    by = "who"
  ) |>
  mutate(outcome = factor(outcome, levels = c(0, 1)))

set.seed(12345)
data_split_wopv <- initial_split(analysis_tibble_wopv, prop = 0.75, strata = outcome)
train_data_wopv <- training(data_split_wopv)
test_data_wopv  <- testing(data_split_wopv)


# ── Make the Recipe ──────────────────────────────────────────────────────────

# ------Note to Nat: see https://www.tmwr.org/pre-proc-table ------

# dummy: step_dummy(one_hot = TRUE) for unordered categoricals,
#        step_dummy(one_hot = FALSE) for per_day (polynomial contrasts),
#        step_ordinalscore() where linear spacing is defensible.
# zv:    step_zv() twice — before and after dummy encoding.
# impute: two-step (step_indicate_na + impute) for MNAR; impute-only for MCAR.
# decorrelate: step_corr(threshold = 0.90).

# -----------------------------------------------------------------

# ── Column groups for the recipe ──────────────────────────────────────────────
#
# Only columns needing special treatment are named here. All remaining numeric
# predictors (every *_days, *_binary, *_streak, age, is_male, is_hispanic,
# alcohol_restraint, polydrug_days, drug_breadth, asi_iv_binary,
# heroin_rbs_alldr_incons, *_inject_days, has_anx_pan_x_benzodiazepine_days,
# rx_*, depression_binary, anxiety_binary, schizophrenia_binary,
# dep_or_anx_binary, has_major_dep, has_epilepsy, has_opiates_dx, excluding
# those removed by step_rm() above) are picked up automatically by
# all_numeric_predictors() in step_normalize and step_corr.

mnar_int_cols_wopv <- c(
  "has_alcol_dx", "has_amphetamines_dx", "has_cannabis_dx",
  "has_cocaine_dx", "has_sedatives_dx",
  "substance_dx_count",
  "is_homeless",
  "is_living_stable"
)

mnar_fct_cols_wopv <- c(
  "job", "education", "marital",
  "housing_stability_ord"
)

mcar_median_int_cols_wopv <- c(
  "is_smoker",
  "has_schizophrenia", "has_bipolar", "has_anx_pan", "has_brain_damage",
  "iv_shared_needles",
  "pain_score",
  "iv_days_total", "iv_events", "iv_amount_dominant",
  "withdrawal_pre", "withdrawal_pre_score",
  "psych_comorbidity_count",
  "has_major_dep_x_withdrawal_pre_score",
  "has_anx_pan_x_withdrawal_pre_score"
)

mcar_mode_fct_cols_wopv <- c(
  "ftnd_score", "per_day"
)

ord_linear_cols_wopv <- c("ftnd_score", "housing_stability_ord")

ord_poly_cols_wopv <- c("per_day")

nominal_unordered_cols_wopv <- c(
  "project", "race", "job", "education", "marital", "site"
)


# ── Recipe ────────────────────────────────────────────────────────────────────

lr_recipe_wopv <- recipe(outcome ~ ., data = train_data_wopv) |>
  update_role(who, new_role = "ID") |>
  step_rm(
    treatment_arm,
    # Leakage: "own"/"other" labeling encodes treatment arm assignment
    took_own_study_drug, took_other_study_drug,
    # Leakage: medication-type flags proxy for treatment arm
    methadone_binary, buprenorphine_binary, suboxone_binary,
    # Separation–rareness: too few users for stable estimation
    muscle_relaxant_days
  ) |>

  # Remove known zero-variance columns before any processing.
  # pcp_*: no PCP use in dataset (all zeros). is_married: bug in building_analysis.R
  # ("married or partnered" != "married"), so it's also all zeros.
  step_zv(all_predictors()) |>

  # ── 00. Imputation ──────────────────────────────────────────────────────────
  # MNAR integers: create na_ind_<col> flag FIRST (preserves missingness signal),
  # then median-impute so downstream steps see no NAs.
  step_indicate_na(all_of(mnar_int_cols_wopv)) |>
  step_impute_median(all_of(mnar_int_cols_wopv)) |>
  # MNAR factors: same two-step approach (indicate then impute).
  step_indicate_na(all_of(mnar_fct_cols_wopv)) |>
  step_impute_mode(all_of(mnar_fct_cols_wopv)) |>
  # MCAR integers → median imputation, no indicator.
  step_impute_median(all_of(mcar_median_int_cols_wopv)) |>
  # MCAR factors → mode imputation, no indicator.
  step_impute_mode(all_of(mcar_mode_fct_cols_wopv)) |>

  # ── 0. Normalization ────────────────────────────────────────────────────────
  step_ordinalscore(all_of(ord_linear_cols_wopv)) |>
  step_normalize(all_numeric_predictors(), -starts_with("na_ind_")) |>

  # ── 1-4. Nominal encoding ────────────────────────────────────────────────────
  # 1. Reserve a level for categories unseen at train time.
  #    per_day excluded: step_novel adds a "new" level, expanding the ordered
  #    factor from 5 to 6 levels before step_dummy(one_hot = FALSE). This pushes
  #    the polynomial basis from degree 4 to 5, producing an extra column the
  #    GLM cannot estimate. step_unknown has the same effect (+1 → degree 6).
  #    NOTE: logistic_regression.R does not yet apply this fix (per_day_4/5/6
  #    appear as NA coefficients there).
  step_novel(all_nominal_predictors(), -all_of(ord_poly_cols_wopv)) |>
  # 2. Pool categories used by < 5% of training rows into "other".
  #    per_day excluded: pooling a rare level would destroy the ordering.
  #    (This exclusion is unchanged from logistic_regression.R.)
  step_other(all_nominal_predictors(), -all_of(ord_poly_cols_wopv), threshold = 0.05) |>
  # 3. Make NA in nominal columns an explicit level. per_day excluded (same reason).
  step_unknown(all_nominal_predictors(), -all_of(ord_poly_cols_wopv)) |>
  # 4a. Unordered factors: one-hot encoding
  step_dummy(all_of(nominal_unordered_cols_wopv), one_hot = TRUE) |>
  # 4b. per_day: polynomial contrasts (preserves nonlinear dose-response shape)
  step_dummy(all_of(ord_poly_cols_wopv), one_hot = FALSE) |>

  # ── Post-encoding ────────────────────────────────────────────────────────────
  step_zv(all_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = 0.90)


# ── Model specification ───────────────────────────────────────────────────────

lr_model_wopv <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")


# ── Workflow ──────────────────────────────────────────────────────────────────

lr_workflow_wopv <- workflow() |>
  add_recipe(lr_recipe_wopv) |>
  add_model(lr_model_wopv)


# ── Fit on full training data ─────────────────────────────────────────────────

lr_fit_wopv <- fit(lr_workflow_wopv, data = train_data_wopv)


# ── Odds ratio table ──────────────────────────────────────────────────────────

lr_or_table_wopv <- lr_fit_wopv |>
  tidy(exponentiate = TRUE) |>
  mutate(
    conf.low  = exp(log(estimate) - 1.96 * std.error),
    conf.high = exp(log(estimate) + 1.96 * std.error)
  ) |>
  filter(term != "(Intercept)", p.value < 0.05) |>
  arrange(desc(abs(log(estimate)))) |>
  select(term, estimate, conf.low, conf.high, p.value)

lr_or_table_wopv |>
  kable(
    digits    = 3,
    col.names = c("Predictor", "Odds Ratio", "95% CI Low", "95% CI High", "p-value"),
    caption   = "LR (no problem vars) odds ratios (p < 0.05, sorted by effect size)."
  )


# ── Training-set metrics ──────────────────────────────────────────────────────

lr_train_preds_wopv <- augment(lr_fit_wopv, new_data = train_data_wopv)

lr_metrics_wopv <- bind_rows(
  accuracy(   lr_train_preds_wopv, truth = outcome, estimate = .pred_class),
  roc_auc(    lr_train_preds_wopv, truth = outcome, .pred_1,        event_level = "second"),
  sensitivity(lr_train_preds_wopv, truth = outcome, estimate = .pred_class, event_level = "second"),
  specificity(lr_train_preds_wopv, truth = outcome, estimate = .pred_class, event_level = "second")
) |>
  select(.metric, .estimate)

lr_metrics_wopv |>
  kable(
    digits    = 3,
    col.names = c("Metric", "Value"),
    caption   = "Training-set performance — no problem variables."
  )


# ── Test-set metrics ──────────────────────────────────────────────────────────

lr_test_preds_wopv <- augment(lr_fit_wopv, new_data = test_data_wopv)

lr_test_metrics_wopv <- bind_rows(
  accuracy(   lr_test_preds_wopv, truth = outcome, estimate = .pred_class),
  roc_auc(    lr_test_preds_wopv, truth = outcome, .pred_1,        event_level = "second"),
  sensitivity(lr_test_preds_wopv, truth = outcome, estimate = .pred_class, event_level = "second"),
  specificity(lr_test_preds_wopv, truth = outcome, estimate = .pred_class, event_level = "second")
) |>
  select(.metric, .estimate)

lr_test_metrics_wopv |>
  kable(
    digits    = 3,
    col.names = c("Metric", "Value"),
    caption   = "Test-set performance — no problem variables."
  )
