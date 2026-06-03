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

# knows what train, outcome, etc are from the prev .R doc
set.seed(12345)
# number of folds isnt a hyperparam, and so it never affects model performance
# and is just chosen, never tuned
# it just affects the variance of ur performance estimate (more folds = lower var)
folds <- vfold_cv(train_data_wopv, v = 10, strata = outcome)

# metric tweak real quick, so we dont have to do event_level = second everywhere
#accuracy is symmetric so we dont need for it

roc_auc2 <- metric_tweak("roc_auc", roc_auc, event_level = "second")
sens2     <- metric_tweak("sens",    sensitivity, event_level = "second")
spec2     <- metric_tweak("spec",    specificity, event_level = "second")

class_metrics <- metric_set(accuracy, roc_auc2, sens2, spec2)

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

# Named metric tibble (CV means) for the comparison table in analysis.qmd. # added 6/2
lr_cv_test_metrics <- rs_results |>
  collect_metrics() |>
  dplyr::transmute(
    method   = "Logistic (10-fold CV, no problem vars)",
    .metric  = dplyr::recode(.metric, sens = "sens", spec = "spec",
                             accuracy = "accuracy", roc_auc = "roc_auc"),
    .estimate = mean
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
    title = "ROC curve — 10-fold CV (gray = individual folds, black = pooled)"
  ) +
  theme_minimal(base_size = 11)
