library(glmnet)      # the LASSO/elastic-net engine
library(vip)         # variable importance — unlike knn, glmnet actually supports this
library(doParallel)
if (!exists("train_data")) {
  source(here::here("vignettes/logistic_regression.R"))
}
# ── Elastic Net Penalty → Pure Ridge ─────────────────────────────────────────
# Everything in THIS section fits an ELASTIC NET logistic regression: both the
# penalty (λ) and the mixture (L1/L2 blend) are tuned over a joint 50×5 grid.
# The cross-validated search selected mixture = 0 — so the best model here is
# PURE RIDGE (L2 only; no coefficient is driven to exactly zero, no feature
# selection). Objects are prefixed `enet_` so they never collide with the Pure
# LASSO section below (both sections can run back-to-back with zero overwriting).

# Workflow ----------------
# Same recipe as the logistic regression with the separation issue (lr_recipe).
# LASSO is actually the *right* tool for that: the L1 penalty shrinks unstable,
# separation-prone coefficients toward (or exactly to) zero, so we WANT the
# messy recipe here — it's where regularization earns its keep.
enet_recipe <- lr_recipe

# elastic net = penalty (lambda, how much shrinkage) + mixture (the L1/L2 blend:
# 1 = pure LASSO, 0 = pure ridge). We tune both. glmnet wants normalized numeric
# predictors with no NAs, which lr_recipe already guarantees.
enet_spec <-
  logistic_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet") |>
  set_mode("classification")

enet_workflow <-
  workflow() |>
  add_recipe(enet_recipe) |>
  add_model(enet_spec)

# Tuning Metric ---
# Use plain roc_auc for tuning/selection, exactly like the course template
# (07_example_lasso.Rmd) and knn.R. Do NOT wrap it in metric_tweak(event_level =
# "second"): roc_auc is direction-agnostic when self-consistent. With the default
# event_level = "first", tune_grid pairs "first level as event" with the first-
# level probability (.pred_0), which gives a PROPER AUC > 0.5 for good models
# (scoring 0s high by .pred_0 is identical to scoring 1s high by .pred_1).
# metric_tweak("...", event_level = "second") instead created a MISMATCH inside
# tune_grid (second-level event paired against the first-level prob) → good models
# scored 1 - AUC ≈ 0.31, so the null model's 0.5 won and penalty = 1 zeroed every
# coefficient (AUC = 0.5, spec = 1, sens = 0 on the test set).
# The final eval calls below pair .pred_1 WITH event_level = "second" — that pairing
# IS self-consistent, so those stay and report the correct AUC.

# Hyperparameter Tuning-----
# grid_regular builds a regular grid: 50 penalty values (log-spaced over dials'
# default 10^-10..10^0) crossed with 5 mixture values (0, .25, .5, .75, 1).
# glmnet fits the whole penalty path in one call per mixture, so this is cheap.
enet_grid <- grid_regular(
  penalty(),
  mixture(),
  levels = c(penalty = 50, mixture = 5)
)

set.seed(12345)
enet_folds <- vfold_cv(train_data, v = 10, strata = outcome)

# parallel backend so the 5 mixtures fan out across cores
enet_cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(enet_cl)
enet_tune <- tune_grid(
  enet_workflow,
  resamples = enet_folds,
  grid      = enet_grid
)
stopCluster(enet_cl)



# Model Fit --------
# select_by_one_std_err = the classic "lambda.1se" rule: take the SIMPLEST model
# whose performance is within one standard error of the best. desc(penalty) tells
# it that higher penalty = simpler (more shrinkage), so it leans toward sparser
# models on purpose — fewer surviving coefficients, less overfitting.
enet_favorite <- select_by_one_std_err(enet_tune, desc(penalty), metric = "roc_auc")
# this outputs a 1-row tibble of penalty, mixture, .config

enet_final_wf <- finalize_workflow(enet_workflow, enet_favorite)

enet_fit <- enet_final_wf |> fit(data = train_data)

# Review Fit on Training Data-----
relapse_pred_enet <-
  predict(enet_fit, train_data, type = "prob") |>
  bind_cols(train_data |> select(outcome))

relapse_pred_enet_class <-
  predict(enet_fit, train_data, type = "class") |>
  bind_cols(train_data |> select(outcome))

# ROC/AUC Plot
relapse_pred_enet |>
  roc_curve(
    truth = outcome,
    .pred_1,
    event_level = "second"
  ) |>
  autoplot()

# ROC/AUC Score table
# roc_auc() returns the single auc number and has one row with .metric/.estimator/.estimate
# roc_curve(), like below was before i changed, is the ugly table with everryyy pt
relapse_pred_enet |>
  roc_auc(
    truth = outcome,
    .pred_1,
    event_level = "second"
  )

# Sensitivity
relapse_pred_enet_class |>
  sens(
    truth       = outcome,
    estimate    = .pred_class,
    event_level = "second"
  )

# Specificity
relapse_pred_enet_class |>
  spec(
    truth       = outcome,
    estimate    = .pred_class,
    event_level = "second"
  )

# Look at Model Metrics -----
# last_fit() fits the final best model to the training set and evaluates the test
# set. Default metrics (accuracy + roc_auc) are self-consistent, so the test
# roc_auc reads correctly without any event_level tweak — same as the template.
enet_last_fit <- enet_final_wf |> last_fit(data_split)

# accuracy and roc_auc on the held-out test set
collect_metrics(enet_last_fit)

# sped pulled out separately — it's the clinically important metric
enet_last_fit |>
  collect_predictions() |>
  specificity(
    truth       = outcome,
    estimate    = .pred_class,
    event_level = "second"
  )

# Look at Variable Importance ------
# Here's where LASSO beats knn: the model IS its coefficients, so importance is
# meaningful. vip() shows the largest |coefficients| at the selected penalty —
# the predictors that survived shrinkage. Anything LASSO zeroed out just won't
# appear, which is the whole selling point (built-in feature selection).
enet_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 20)

# If you'd rather see the raw surviving coefficients (incl. which got zeroed),
# this gives the glmnet coefficient table at the chosen lambda:
enet_fit |>
  extract_fit_parsnip() |>
  tidy()

# Review Fit on the Test Data ------
relapse_pred_enet_test <-
  predict(enet_fit, test_data, type = "prob") |>
  bind_cols(test_data |> select(outcome))

relapse_pred_enet_test |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot()


# ── Pure LASSO ───────────────────────────────────────────────────────────────
# Same as the Elastic Net section above, but mixture is fixed to 1 (pure LASSO),
# so the grid tunes penalty only. Everything else is identical with lasso_ names.

# Workflow ----------------
lasso_recipe <- lr_recipe

lasso_spec <-
  logistic_reg(penalty = tune(), mixture = 1) |>   # mixture = 1 -> pure LASSO
  set_engine("glmnet") |>
  set_mode("classification")

lasso_workflow <-
  workflow() |>
  add_recipe(lasso_recipe) |>
  add_model(lasso_spec)

# Hyperparameter Tuning-----
lasso_grid <- grid_regular(penalty(), levels = 50)

set.seed(12345)
lasso_folds <- vfold_cv(train_data, v = 10, strata = outcome)

lasso_cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(lasso_cl)
lasso_tune <- tune_grid(
  lasso_workflow,
  resamples = lasso_folds,
  grid      = lasso_grid
)
stopCluster(lasso_cl)

# Model Fit --------
lasso_favorite <- select_by_one_std_err(lasso_tune, desc(penalty), metric = "roc_auc")

lasso_final_wf <- finalize_workflow(lasso_workflow, lasso_favorite)

lasso_fit <- lasso_final_wf |> fit(data = train_data)

# Review Fit on Training Data-----
relapse_pred_lasso <-
  predict(lasso_fit, train_data, type = "prob") |>
  bind_cols(train_data |> select(outcome))

relapse_pred_lasso_class <-
  predict(lasso_fit, train_data, type = "class") |>
  bind_cols(train_data |> select(outcome))

relapse_pred_lasso |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot()

relapse_pred_lasso |>
  roc_auc(truth = outcome, .pred_1, event_level = "second")

relapse_pred_lasso_class |>
  sens(truth = outcome, estimate = .pred_class, event_level = "second")

relapse_pred_lasso_class |>
  spec(truth = outcome, estimate = .pred_class, event_level = "second")

# Look at Model Metrics -----
lasso_last_fit <- lasso_final_wf |> last_fit(data_split)

collect_metrics(lasso_last_fit)

lasso_last_fit |>
  collect_predictions() |>
  specificity(truth = outcome, estimate = .pred_class, event_level = "second")

# Look at Variable Importance ------
lasso_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 20)

lasso_fit |>
  extract_fit_parsnip() |>
  tidy()

# Review Fit on the Test Data ------
relapse_pred_lasso_test <-
  predict(lasso_fit, test_data, type = "prob") |>
  bind_cols(test_data |> select(outcome))

relapse_pred_lasso_test |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot()

