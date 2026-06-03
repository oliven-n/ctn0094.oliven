# logistic_regression_wopv_kfold.R
#
# K-fold cross-validated logistic regression using the without-problem-variables
# recipe. Sourced by analysis.qmd after logistic_regression_without_problem_vars.R.
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

if (!exists("lr_workflow_wopv")) {
  source(here::here("vignettes/logistic_regression_without_problem_vars.R"))
}
# youden_cutoff() and sens_spec_at() live in metrics_helpers.R
if (!exists("youden_cutoff")) {
  source(here::here("vignettes/metrics_helpers.R"))
}

# knows what train, outcome, etc are from the prev .R doc
set.seed(12345)
# number of folds isnt a hyperparam, and so it never affects model performance
# and is just chosen, never tuned
# it just affects the variance of ur performance estimate (more folds = lower var)
folds <- vfold_cv(train_data_wopv, v = 5, strata = outcome)

# metric_tweak("roc_auc", roc_auc, event_level = "second") produces a reflected
# (1 - correct) AUC inside fit_resamples — do not use it. Use plain roc_auc
# (direction-agnostic: scoring "0"s high via .pred_0 == scoring "1"s high via
# .pred_1 for AUC). Build lr_cv_test_metrics from collect_predictions() with
# explicit event_level = "second", matching every other method in the table.
class_metrics <- metric_set(accuracy, roc_auc)

# fit (fit means train on data) resamples
#fit_resamples() is for performance estimation only
# tune_grid() is for finding the best hyperparams (we dont need that here)
rs_results <- lr_workflow_wopv |>
  fit_resamples(
    resamples = folds,
    metrics   = class_metrics,
    control   = control_resamples(save_pred = TRUE)
  )

stopCluster(cl)

#inspect results
rs_results |> collect_metrics(summarize = FALSE)
rs_results |> collect_metrics()

# Named metric tibble for the comparison table in analysis.qmd. # added 6/2
# Built from collect_predictions() so event_level = "second" is explicit.
# Youden-J cutoff on pooled held-out predictions (analogous to train-chosen cutoff).
cv_preds  <- collect_predictions(rs_results)
lr_cv_cut <- youden_cutoff(cv_preds)

lr_cv_test_metrics <- dplyr::bind_rows(
  roc_auc( cv_preds, truth = outcome, .pred_1, event_level = "second"),
  accuracy(cv_preds, truth = outcome, estimate = .pred_class),
  sens_spec_at(cv_preds, lr_cv_cut)
) |>
  dplyr::transmute(
    method    = "Logistic (5-fold CV, no problem vars)",
    .metric,
    .estimate
  )

# roc curve
rs_results |>
  collect_predictions() |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot()

# split by fold
rs_results |>
  collect_predictions() |>
  group_by(id) |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot()

# superimposed: per-fold curves (gray) + pooled overall curve (black)
fold_curves    <- rs_results |>
  collect_predictions() |>
  group_by(id) |>
  roc_curve(truth = outcome, .pred_1, event_level = "second")

overall_curve  <- rs_results |>
  collect_predictions() |>
  roc_curve(truth = outcome, .pred_1, event_level = "second")

ggplot() +
  geom_path(
    data = fold_curves,
    aes(x = 1 - specificity, y = sensitivity, group = id),
    color = "gray70", linewidth = 0.5, alpha = 0.7
  ) +
  geom_path(
    data = overall_curve,
    aes(x = 1 - specificity, y = sensitivity),
    color = "black", linewidth = 1.2
  ) +
  geom_abline(lty = 3, color = "gray40") +
  coord_equal() +
  labs(
    x = "1 - Specificity",
    y = "Sensitivity",
    title = "ROC curve — 5-fold CV (gray = individual folds, black = pooled)"
  ) +
  theme_minimal(base_size = 11)
