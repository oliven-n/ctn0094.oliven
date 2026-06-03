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
the_folds <- vfold_cv(train_data_wopv, v = 10, strata = outcome)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)
kknn_tune <- tune_grid(kknn_workflow, resamples = the_folds, grid = knn_grid)
stopCluster(cl)


# Model Fit --------
favorite <- select_by_one_std_err(kknn_tune, neighbors, metric = "roc_auc")

final_wf <- finalize_workflow(kknn_workflow, favorite)

knn_fit <- final_wf |> fit(data = train_data_wopv)

# Review Fit on Training Data-----
relapse_pred_knn <-
  predict(knn_fit, train_data_wopv, type = "prob") |>
  bind_cols(train_data_wopv |> select(outcome))

# ROC/AUC Plot
relapse_pred_knn |>
  roc_curve(
    truth = outcome,
    .pred_1,
    event_level = "second"
  ) |>
  autoplot()

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

# Look at Model Metrics -----
knn_last_fit <- final_wf |> last_fit(data_split_wopv)

# accuracy and roc_auc on test set (tidymodels defaults)
collect_metrics(knn_last_fit)

# Test sens + spec at the SAME train-chosen cutoff (choose on train, report on test)
sens_spec_at(collect_predictions(knn_last_fit), knn_cut)

# Named metric tibble for the cross-method comparison table in analysis.qmd. # added 6/2
knn_wopv_test_metrics <- test_metrics_from_lastfit(
  knn_last_fit, knn_cut, "KNN (no problem vars)"
)

# Review Fit on the Test Data ------
relapse_pred_knn_test <-
  predict(knn_fit, test_data_wopv, type = "prob") |>
  bind_cols(test_data_wopv |> select(outcome))

relapse_pred_knn_test |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot()
