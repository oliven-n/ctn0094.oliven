library(kknn)
library(doParallel)
if (!exists("train_data")) {
  source(here::here("vignettes/logistic_regression.R"))
}
source(here::here("vignettes/metrics_helpers.R"))

# Workflow ----------------
# pipe it over, the version with the separation issue bc we hope knn can handle
kknn_recipe <- lr_recipe

# i used parsnip_addin() - gotta run it in console - to get this sample code block
kknn_spec <-
  # took off weight_func = tune(), dist_power = tune() from below as args to get fast
  nearest_neighbor(neighbors = tune()) |>
  # the order of the below two doesnt matter and tbh im not sure why
  set_engine('kknn') |>
  set_mode('classification')

# can't pipe over the workflow because we have funn special thingz like kknn_spec
kknn_workflow <-
  workflow() |>
  add_recipe(kknn_recipe) |>
  add_model(kknn_spec)

# Hyperparameter Tuning-----
# (in our case there is just the one for now)
knn_grid <-
  data.frame(neighbors = seq(5, 50, by = 5))

set.seed(12345)
the_folds <- vfold_cv(train_data, v = 10, strata = outcome)

# tune_grid returns a tibble where each row is a resample x hyperparam combo,
# with a .metrics list column
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)
kknn_tune <- tune_grid(kknn_workflow, resamples = the_folds, grid = knn_grid)
stopCluster(cl)


# Model Fit --------

# the shrinkage estiamte (selecting k)
# more k, more shrinkage
# why are we selecting by one_std_err
# what are some other ways to select the best k
favorite <- select_by_one_std_err(kknn_tune, neighbors, metric = "roc_auc")
# this outputs a 1x2 tibble of neighbors,.config


final_wf <- finalize_workflow(kknn_workflow, favorite)

knn_fit <- final_wf |> fit(data=train_data)

# Review Fit on Training Data-----
relapse_pred_knn <-
  predict(knn_fit, train_data, type = "prob") |>
  bind_cols(train_data |> select(outcome))

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

# last_fit() fits the final best model to the training set and evaluates the test set
knn_last_fit <- final_wf |> last_fit(data_split)

# accuracy and roc_auc on test set (tidymodels defaults)
collect_metrics(knn_last_fit)

# Test sens + spec at the SAME train-chosen cutoff (choose on train, report on test)
sens_spec_at(collect_predictions(knn_last_fit), knn_cut)

# Named metric tibble for the cross-method comparison table in analysis.qmd. # added 6/2
knn_test_metrics <- test_metrics_from_lastfit(
  knn_last_fit, knn_cut, "KNN"
)

# Look at Variable Importance ------
# KNN has no native variable importance (see the example doc: "Most modeling
# methods (not knn) include statistics on the relative importance of each
# predictor"). vip() only works on models that expose importance, so we skip it.

# Review Fit on the Test Data ------

relapse_pred_knn_test <-
  predict(knn_fit, test_data, type = "prob") |>
  bind_cols(test_data |> select(outcome))

relapse_pred_knn_test |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot()

