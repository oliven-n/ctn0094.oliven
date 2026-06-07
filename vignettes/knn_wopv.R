library(kknn)
library(doParallel)
if (!exists("train_data_wopv")) {
  source(here::here("vignettes/logistic_regression_without_problem_vars.R"))
}
source(here::here("vignettes/metrics_helpers.R"))

# Workflow ----------------
# Same KNN setup as knn.R, but built on the without-problem-variables recipe
# (lr_recipe_wopv): leakage + separation-prone predictors removed.
kknn_recipe <- lr_recipe_wopv

kknn_spec <-
  nearest_neighbor(neighbors = tune()) |>
  set_engine('kknn') |>
  set_mode('classification')

kknn_workflow <-
  workflow() |>
  add_recipe(kknn_recipe) |>
  add_model(kknn_spec)

# Hyperparameter Tuning-----
knn_grid <-
  data.frame(neighbors = seq(5, 50, by = 5))

set.seed(12345)
the_folds <- vfold_cv(train_data_wopv, v = 5, strata = outcome)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)
kknn_tune <- tune_grid(kknn_workflow, resamples = the_folds, grid = knn_grid)
stopCluster(cl)

favorite <- select_by_one_std_err(kknn_tune, neighbors, metric = "roc_auc")

# Model Fit --------
final_wf <- finalize_workflow(kknn_workflow, favorite)

knn_fit <- final_wf |> fit(data = train_data_wopv)

# CV Metrics --------
# Divergence: tune_grid fit on 5-fold subsets of train_data_wopv (each fold holds out
# 1/5 for validation). The final model below is a fresh fit on ALL train_data_wopv.
# mean = average of the 5 fold-level roc_auc .estimates tune_grid computed internally;
# std_err = sd of those estimates / sqrt(5); n = 5.
# sens/spec omitted here: not available from tune_grid without save_pred=TRUE.
knn_wopv_cv_metrics <- collect_metrics(kknn_tune) |>
  dplyr::filter(.metric == "roc_auc", neighbors == favorite$neighbors) |>
  dplyr::select(.metric, mean, std_err, n)
knn_wopv_cv_metrics

# Review Fit on Training Data-----
relapse_pred_knn <-
  predict(knn_fit, train_data_wopv, type = "prob") |>
  bind_cols(train_data_wopv |> select(outcome))

# ROC/AUC Plot (train) — enriched with title/labels. # enriched 6/2
relapse_pred_knn |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "KNN (no problem vars): training ROC curve",
    subtitle = "Relapse as the positive class; neighbors selected by 1-SE rule",
    x = "1 - Specificity", y = "Sensitivity"
  )

# ROC/AUC Score table
relapse_pred_knn |>
  roc_curve(
    truth = outcome,
    .pred_1,
    event_level = "second"
  )

# Sensitivity & Specificity at the Youden-J cutoff (chosen on TRAIN, not 0.5),
# matching logistic_enet.R so analysis.qmd reports a consistent operating point.
knn_cut <- youden_cutoff(relapse_pred_knn)
knn_cut

sens_spec_at(relapse_pred_knn, knn_cut)

# Train Metrics --------
# Divergence: knn_fit was trained on ALL of train_data_wopv (not CV folds).
# relapse_pred_knn is in-sample prediction on the same train_data_wopv — intentionally
# optimistic, equivalent to Balise et al.'s "Full Training Dataset" column.
knn_wopv_train_metrics <- dplyr::bind_rows(
  roc_auc(relapse_pred_knn, truth = outcome, .pred_1, event_level = "second"),
  sens_spec_at(relapse_pred_knn, knn_cut)
) |> dplyr::select(.metric, .estimate)
knn_wopv_train_metrics

# Test Metrics --------
# Divergence: last_fit fits on training split of data_split_wopv, evaluates on test.
knn_last_fit <- final_wf |> last_fit(data_split_wopv)

# accuracy and roc_auc on test set (tidymodels defaults)
collect_metrics(knn_last_fit)

# Test sens + spec at the SAME train-chosen cutoff (choose on train, report on test)
sens_spec_at(collect_predictions(knn_last_fit), knn_cut)

# Named metric tibble for the cross-method comparison table in analysis.qmd. # added 6/2
knn_wopv_test_metrics <- test_metrics_from_lastfit(
  knn_last_fit, knn_cut, "KNN (no problem vars)"
)

# collect_predictions(knn_last_fit) reuses the test-split predictions already
# computed inside last_fit() above — identical to predict(knn_fit, test_data_wopv)
# but avoids a redundant prediction call and keeps this plot consistent with the
# scalar metrics derived from the same last_fit object.
relapse_pred_knn_test <- collect_predictions(knn_last_fit)

# enriched 6/2
relapse_pred_knn_test |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "KNN (no problem vars): test ROC curve",
    subtitle = "Held-out 25% test split; relapse as the positive class",
    x = "1 - Specificity", y = "Sensitivity"
  )

# Look at Variable Importance ------
# KNN has no intrinsic coefficients, so we use model-agnostic PERMUTATION
# importance: shuffle one predictor at a time and measure the drop in test ROC
# AUC. A bigger drop = a more important predictor. # added 6/2
library(vip)

knn_auc_metric <- function(truth, estimate) {
  yardstick::roc_auc_vec(truth = truth, estimate = estimate, event_level = "second")
}

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)
set.seed(12345)
knn_wopv_vip_obj <- vip::vi_permute(
  object        = knn_fit,
  feature_names = setdiff(names(train_data_wopv), c("who", "outcome")),
  train         = train_data_wopv,
  target        = "outcome",
  metric        = knn_auc_metric,
  smaller_is_better = FALSE,
  pred_wrapper  = function(object, newdata) {
    # vip subsets newdata to feature_names, dropping the `who` ID column the
    # recipe needs; re-add a dummy (who has role "ID", so it never affects preds).
    if (!"who" %in% names(newdata)) newdata$who <- 1L
    predict(object, newdata, type = "prob")$.pred_1
  },
  nsim = 10
)
stopCluster(cl)

vip::vip(knn_wopv_vip_obj, num_features = 15, geom = "col") +
  ggplot2::labs(
    title    = "KNN (no problem vars) permutation variable importance",
    subtitle = "Mean drop in test ROC AUC when each predictor is shuffled (10 permutations)",
    x        = "Importance (mean ROC AUC decrease)",
    y        = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11)
