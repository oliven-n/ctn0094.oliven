library(glmnet)      # the LASSO/elastic-net engine
library(vip)         # variable importance — unlike knn, glmnet actually supports this
library(doParallel)
if (!exists("train_data")) {
  source(here::here("vignettes/logistic_regression.R"))
}
source(here::here("vignettes/metrics_helpers.R"))

# ── Helpers (shared by both sections) ────────────────────────────────────────
# 0.5 is a useless classification cutoff here: the ~74% relapse base rate pushes
# nearly all predicted probabilities above 0.5, so a 0.5 threshold labels almost
# everyone "relapse" (sens ≈ 1, spec ≈ 0). Instead we choose the Youden-J cutoff
# (the threshold maximizing sens + spec - 1) on the TRAINING ROC curve, then apply
# that SAME cutoff to both train and test (choose on train, report on test).

# youden_cutoff() and sens_spec_at() now live in metrics_helpers.R (sourced
# above) so this file and the knn scripts share one definition. # enriched 6/2

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
enet_folds <- vfold_cv(train_data, v = 5, strata = outcome)

# parallel backend so the 5 mixtures fan out across cores
enet_cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(enet_cl)
enet_tune <- tune_grid(
  enet_workflow,
  resamples = enet_folds,
  grid      = enet_grid
)
stopCluster(enet_cl)

# select_by_one_std_err = the classic "lambda.1se" rule: take the SIMPLEST model
# whose performance is within one standard error of the best. desc(penalty) tells
# it that higher penalty = simpler (more shrinkage), so it leans toward sparser
# models on purpose — fewer surviving coefficients, less overfitting.
enet_favorite <- select_by_one_std_err(enet_tune, desc(penalty), metric = "roc_auc")
# this outputs a 1-row tibble of penalty, mixture, .config

# Model Fit --------
enet_final_wf <- finalize_workflow(enet_workflow, enet_favorite)

enet_fit <- enet_final_wf |> fit(data = train_data)

# CV Metrics --------
# Divergence: tune_grid fit on 5-fold subsets of train_data (enet_folds; each fold holds
# out 1/5 for validation). The final model below is a fresh fit on ALL train_data.
# mean = average of the 5 fold-level roc_auc .estimates tune_grid computed internally;
# std_err = sd of those estimates / sqrt(5); n = 5.
# sens/spec omitted here: not available from tune_grid without save_pred=TRUE.
enet_cv_metrics <- collect_metrics(enet_tune) |>
  dplyr::filter(
    .metric == "roc_auc",
    penalty == enet_favorite$penalty,
    mixture == enet_favorite$mixture
  ) |>
  dplyr::select(.metric, mean, std_err, n)
enet_cv_metrics

# Train Metrics --------
# Divergence: enet_fit was trained on ALL of train_data (not CV folds).
# relapse_pred_enet is in-sample prediction on the same train_data — intentionally
# optimistic, equivalent to Balise et al.'s "Full Training Dataset" column.
relapse_pred_enet <-
  predict(enet_fit, train_data, type = "prob") |>
  bind_cols(train_data |> select(outcome))

# Sensitivity & Specificity at the Youden-J cutoff (chosen on TRAIN, not 0.5)
enet_cut <- youden_cutoff(relapse_pred_enet)
enet_cut

enet_train_metrics <- dplyr::bind_rows(
  roc_auc(relapse_pred_enet, truth = outcome, .pred_1, event_level = "second"),
  sens_spec_at(relapse_pred_enet, enet_cut)
) |> dplyr::select(.metric, .estimate)
enet_train_metrics

# Review Fit on Training Data --------
# Visualization of in-sample fit. relapse_pred_enet and enet_cut defined above.

# ROC/AUC Plot (train) — enriched with title/labels. # enriched 6/2
relapse_pred_enet |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "Elastic net (ridge): training ROC curve",
    subtitle = "Relapse as the positive class; 28-day pre-treatment predictors",
    x = "1 - Specificity", y = "Sensitivity"
  )

# ROC/AUC Score table
# roc_auc() returns the single auc number and has one row with .metric/.estimator/.estimate
# roc_curve(), like below was before i changed, is the ugly table with everryyy pt
relapse_pred_enet |>
  roc_auc(
    truth = outcome,
    .pred_1,
    event_level = "second"
  )

# Test Metrics --------
# Divergence: last_fit fits on training split of data_split, evaluates on test.
# last_fit() fits the final best model to the training set and evaluates the test
# set. Default metrics (accuracy + roc_auc) are self-consistent, so the test
# roc_auc reads correctly without any event_level tweak — same as the template.
enet_last_fit <- enet_final_wf |> last_fit(data_split)

# accuracy and roc_auc on the held-out test set
collect_metrics(enet_last_fit)

# Test sens + spec at the SAME train-chosen cutoff (choose on train, report on
# test — never tuned on test). spec is the clinically important one here.
sens_spec_at(collect_predictions(enet_last_fit), enet_cut)

# Named metric tibble for the cross-method comparison table in analysis.qmd. # added 6/2
enet_test_metrics <- test_metrics_from_lastfit(
  enet_last_fit, enet_cut, "Elastic net (ridge)"
)

# Review Fit on Test Data --------
# collect_predictions(enet_last_fit) reuses the test-split predictions already
# computed inside last_fit() above — identical to predict(enet_fit, test_data)
# but avoids a redundant prediction call and keeps this plot consistent with the
# scalar metrics derived from the same last_fit object.
relapse_pred_enet_test <- collect_predictions(enet_last_fit)

relapse_pred_enet_test |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "Elastic net (ridge): test ROC curve",
    subtitle = "Held-out 25% test split; relapse as the positive class",
    x = "1 - Specificity", y = "Sensitivity"
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
lasso_folds <- vfold_cv(train_data, v = 5, strata = outcome)

lasso_cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(lasso_cl)
lasso_tune <- tune_grid(
  lasso_workflow,
  resamples = lasso_folds,
  grid      = lasso_grid
)
stopCluster(lasso_cl)

# select_by_one_std_err = the classic "lambda.1se" rule: take the SIMPLEST model
# whose performance is within one standard error of the best. desc(penalty) tells
# it that higher penalty = simpler (more shrinkage), so it leans toward sparser
# models on purpose — fewer surviving coefficients, less overfitting.
lasso_favorite <- select_by_one_std_err(lasso_tune, desc(penalty), metric = "roc_auc")
# this outputs a 1-row tibble of penalty, .config

# Model Fit --------
lasso_final_wf <- finalize_workflow(lasso_workflow, lasso_favorite)

lasso_fit <- lasso_final_wf |> fit(data = train_data)

# CV Metrics --------
# Divergence: tune_grid fit on 5-fold subsets of train_data (lasso_folds; each fold holds
# out 1/5 for validation). The final model below is a fresh fit on ALL train_data.
# mean = average of the 5 fold-level roc_auc .estimates tune_grid computed internally;
# std_err = sd of those estimates / sqrt(5); n = 5.
# sens/spec omitted here: not available from tune_grid without save_pred=TRUE.
lasso_cv_metrics <- collect_metrics(lasso_tune) |>
  dplyr::filter(.metric == "roc_auc", penalty == lasso_favorite$penalty) |>
  dplyr::select(.metric, mean, std_err, n)
lasso_cv_metrics

# Train Metrics --------
# Divergence: lasso_fit was trained on ALL of train_data (not CV folds).
# relapse_pred_lasso is in-sample prediction on the same train_data — intentionally
# optimistic, equivalent to Balise et al.'s "Full Training Dataset" column.
relapse_pred_lasso <-
  predict(lasso_fit, train_data, type = "prob") |>
  bind_cols(train_data |> select(outcome))

# Sensitivity & Specificity at the Youden-J cutoff (chosen on TRAIN, not 0.5)
lasso_cut <- youden_cutoff(relapse_pred_lasso)
lasso_cut

lasso_train_metrics <- dplyr::bind_rows(
  roc_auc(relapse_pred_lasso, truth = outcome, .pred_1, event_level = "second"),
  sens_spec_at(relapse_pred_lasso, lasso_cut)
) |> dplyr::select(.metric, .estimate)
lasso_train_metrics

# Review Fit on Training Data --------
# Visualization of in-sample fit. relapse_pred_lasso and lasso_cut defined above.

# ROC/AUC Plot (train) — enriched with title/labels. # enriched 6/2
relapse_pred_lasso |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "Pure LASSO: training ROC curve",
    subtitle = "Relapse as the positive class; 28-day pre-treatment predictors",
    x = "1 - Specificity", y = "Sensitivity"
  )

# ROC/AUC Score table
# roc_auc() returns the single auc number and has one row with .metric/.estimator/.estimate
# roc_curve(), like below was before i changed, is the ugly table with everryyy pt
relapse_pred_lasso |>
  roc_auc(
    truth = outcome,
    .pred_1,
    event_level = "second"
  )

# Test Metrics --------
# Divergence: last_fit fits on training split of data_split, evaluates on test.
# last_fit() fits the final best model to the training set and evaluates the test
# set. Default metrics (accuracy + roc_auc) are self-consistent, so the test
# roc_auc reads correctly without any event_level tweak — same as the template.
lasso_last_fit <- lasso_final_wf |> last_fit(data_split)

# accuracy and roc_auc on the held-out test set
collect_metrics(lasso_last_fit)

# Test sens + spec at the SAME train-chosen cutoff (choose on train, report on
# test — never tuned on test). spec is the clinically important one here.
sens_spec_at(collect_predictions(lasso_last_fit), lasso_cut)

# Named metric tibble for the cross-method comparison table in analysis.qmd. # added 6/2
lasso_test_metrics <- test_metrics_from_lastfit(
  lasso_last_fit, lasso_cut, "LASSO"
)

# Review Fit on Test Data --------
# collect_predictions(lasso_last_fit) reuses the test-split predictions already
# computed inside last_fit() above — identical to predict(lasso_fit, test_data)
# but avoids a redundant prediction call and keeps this plot consistent with the
# scalar metrics derived from the same last_fit object.
relapse_pred_lasso_test <- collect_predictions(lasso_last_fit)

relapse_pred_lasso_test |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "Pure LASSO: test ROC curve",
    subtitle = "Held-out 25% test split; relapse as the positive class",
    x = "1 - Specificity", y = "Sensitivity"
  )

# Look at Variable Importance ------
# vip() does not pin to the workflow's selected penalty — it reads a different
# lambda on the glmnet path and shows more predictors than actually survived.
# Instead, extract coefficients AT the selected penalty so importance correctly
# reflects the sparse model that produced the reported AUC.
lasso_coef <- tibble::tibble(
  term     = rownames(coef(extract_fit_engine(lasso_fit), s = lasso_favorite$penalty)),
  estimate = as.vector(coef(extract_fit_engine(lasso_fit), s = lasso_favorite$penalty))
) |>
  dplyr::filter(term != "(Intercept)", estimate != 0) |>
  dplyr::mutate(importance = abs(estimate)) |>
  dplyr::arrange(dplyr::desc(importance))

ggplot(lasso_coef, aes(x = importance, y = reorder(term, importance))) +
  geom_col(fill = "grey40") +
  labs(x = "Importance (|coefficient| at selected λ)",
       y = NULL,
       title = paste0("Pure LASSO: ", nrow(lasso_coef),
                      " surviving predictors (of ~107)")) +
  theme_minimal()

lasso_coef   # raw coefficient table for the surviving terms


# ── Shrinkage Plots (given mixture settings) ─────────────────────────────────
# Lambda-axis diagnostics. One glmnet path fit (at a chosen mixture) gives BOTH
# the per-variable coefficient paths and the per-lambda metric paths.
library(glmnet)
library(broom)
library(plotly)
conflicts_prefer(plotly::layout)

# Bake the recipe once -> numeric predictor matrix + outcome vector for glmnet.
# (who has role "ID" and outcome is dropped by all_predictors(), so X is clean.)
bake_xy <- function(prepped, data) {
  list(
    X = bake(prepped, new_data = data, all_predictors(), composition = "matrix"),
    y = bake(prepped, new_data = data, all_outcomes()) |> dplyr::pull(1)
  )
}

enet_prep <- prep(enet_recipe, training = train_data)
enet_tr   <- bake_xy(enet_prep, train_data)
enet_te   <- bake_xy(enet_prep, test_data)

# glmnet paths at each selected mixture (alpha = mixture): 0 = ridge, 1 = lasso.
enet_path  <- glmnet(enet_tr$X, enet_tr$y, family = "binomial", alpha = 0)
lasso_path <- glmnet(enet_tr$X, enet_tr$y, family = "binomial", alpha = 1)

# Coefficient Paths (ISLR style) ----
# Highlight top 5 terms by max |coef|, grey the rest; dashed line at chosen lambda.
plot_coef_paths <- function(glmnet_fit, chosen_penalty, title) {
  coef_df <- tidy(glmnet_fit) |> dplyr::filter(term != "(Intercept)")
  top_terms <- coef_df |>
    dplyr::group_by(term) |>
    dplyr::summarise(m = max(abs(estimate)), .groups = "drop") |>
    dplyr::slice_max(m, n = 5) |>
    dplyr::pull(term)
  coef_df <- dplyr::mutate(coef_df, highlight = ifelse(term %in% top_terms, term, "other"))
  ggplot() +
    geom_line(data = dplyr::filter(coef_df, highlight == "other"),
              aes(lambda, estimate, group = term), colour = "grey80", linewidth = 0.3) +
    geom_line(data = dplyr::filter(coef_df, highlight != "other"),
              aes(lambda, estimate, colour = highlight, group = term), linewidth = 0.9) +
    scale_x_log10() +
    scale_colour_manual(values = c("#D55E00", "#0072B2", "#E69F00", "#009E73", "#CC79A7")) +
    geom_vline(xintercept = chosen_penalty, linetype = "dashed") +
    labs(x = "Penalty (lambda, log scale)", y = "Standardized coefficient",
         colour = "Top |coef|", title = title) +
    theme_minimal()
}

# Ridge (mixture 0): smooth shrinkage, nothing hits exactly 0.
plot_coef_paths(enet_path, enet_favorite$penalty,
                "Elastic net (mixture = 0, ridge): coefficient paths")
# Pure LASSO (mixture 1): the ISLR picture - terms peel off to exactly 0.
plot_coef_paths(lasso_path, lasso_favorite$penalty,
                "Pure LASSO (mixture = 1): coefficient paths")

# Metric Paths vs Lambda (train vs test) ----
# At each lambda: roc_auc (threshold-free) + sens/spec read at the Youden-J cutoff
# CHOSEN ON TRAIN at that lambda, then applied to both sets. roc_auc ignores the
# cutoff; sens/spec do not.
enet_p_tr <- predict(enet_path, enet_tr$X, type = "response")  # [n x nlambda] = P(relapse)
enet_p_te <- predict(enet_path, enet_te$X, type = "response")
enet_lams <- enet_path$lambda

# per-lambda Youden cutoff, chosen on TRAIN
enet_path_cuts <- vapply(seq_along(enet_lams), function(j) {
  tibble::tibble(truth = enet_tr$y, .p = enet_p_tr[, j]) |>
    roc_curve(truth, .p, event_level = "second") |>
    dplyr::mutate(jx = sensitivity + specificity - 1) |>
    dplyr::slice_max(jx, n = 1, with_ties = FALSE) |>
    dplyr::pull(.threshold)
}, numeric(1))

metrics_along_path <- function(p_mat, y, cuts, lams, set_label) {
  purrr::map_dfr(seq_along(lams), function(j) {
    p   <- p_mat[, j]
    lab <- factor(dplyr::if_else(p >= cuts[j], "1", "0"), levels = c("0", "1"))
    tibble::tibble(
      lambda  = lams[j],
      set     = set_label,
      roc_auc = yardstick::roc_auc_vec(y, p,   event_level = "second"),
      sens    = yardstick::sens_vec(   y, lab, event_level = "second"),
      spec    = yardstick::spec_vec(   y, lab, event_level = "second")
    )
  })
}

enet_metric_df <- dplyr::bind_rows(
  metrics_along_path(enet_p_tr, enet_tr$y, enet_path_cuts, enet_lams, "Training"),
  metrics_along_path(enet_p_te, enet_te$y, enet_path_cuts, enet_lams, "Testing")
) |>
  tidyr::pivot_longer(c(roc_auc, sens, spec), names_to = "metric", values_to = "value")

# Train (left) and Test (right), same colour per metric, dashed line at selected lambda.
# NB: the test panel is ILLUSTRATIVE - lambda and the cutoff were chosen on train.
ggplot(enet_metric_df, aes(lambda, value, colour = metric)) +
  geom_line(linewidth = 0.8) +
  scale_x_log10() +
  geom_vline(xintercept = enet_favorite$penalty, linetype = "dashed") +
  facet_wrap(~ set) +
  labs(x = "Penalty (lambda, log scale)", y = "Metric value", colour = "Metric",
       title = "Metric stability vs shrinkage (mixture = 0): Training vs Testing",
       subtitle = "sens/spec at the per-lambda Youden-J cutoff (chosen on train)") +
  theme_minimal()

# 3D Surface: penalty x mixture x CV roc_auc (rotatable in the Viewer) ----
enet_surface <- collect_metrics(enet_tune) |>
  dplyr::filter(.metric == "roc_auc") |>
  dplyr::select(penalty, mixture, mean)

z_mat <- enet_surface |>
  tidyr::pivot_wider(names_from = mixture, values_from = mean) |>
  tibble::column_to_rownames("penalty") |>
  as.matrix()
penalties <- as.numeric(rownames(z_mat))
mixtures  <- as.numeric(colnames(z_mat))

enet_best <- select_best(enet_tune, metric = "roc_auc")
sel_pts <- dplyr::bind_rows(
  dplyr::mutate(enet_favorite, rule = "select_by_one_std_err"),
  dplyr::mutate(enet_best,     rule = "select_best")
) |>
  dplyr::left_join(enet_surface, by = c("penalty", "mixture"))

# Note: the LASSO path (mixture = 1) lies on the right-hand boundary plane of
# the surface. Plotly renders it at the edge but it is correctly included in
# the z_mat grid column for mixture = 1. # added 6/2
plot_ly() |>
  add_surface(x = ~mixtures, y = ~log10(penalties), z = ~z_mat,
              colorscale = "Viridis", opacity = 0.85,
              colorbar = list(title = "CV roc_auc")) |>
  add_markers(data = sel_pts,
              x = ~mixture, y = ~log10(penalty), z = ~mean,
              color = ~rule, size = I(140),
              text = ~paste0(rule,
                             "<br>penalty = ", signif(penalty, 3),
                             "<br>mixture = ", mixture,
                             "<br>CV roc_auc = ", round(mean, 4)),
              hoverinfo = "text") |>
  layout(title = "CV roc_auc over (penalty, mixture)",
         scene = list(
           xaxis = list(title = "mixture"),
           yaxis = list(title = "log10(penalty)"),
           zaxis = list(title = "CV roc_auc")))

