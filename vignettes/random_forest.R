library(ranger)
library(doParallel)
if (!exists("train_data")) {
  source(here::here("vignettes/logistic_regression.R"))
}
source(here::here("vignettes/metrics_helpers.R"))


# Workflow ----------------
# Random forest uses the same shared recipe as the other models (lr_recipe).
randfor_recipe <- lr_recipe

randfor_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |>
  # importance = 'impurity' required at engine time: ranger does not compute or
  # store variable importance by default (opt-in). Without this flag, vip::vip()
  # errors because $variable.importance is not present on the fitted ranger object.
  set_engine('ranger', importance = 'impurity') |>
  set_mode('classification')

# can't pipe over the workflow because we have funn special thingz like randfor_spec
randfor_workflow <-
  workflow() |>
  add_recipe(randfor_recipe) |>
  add_model(randfor_spec)

# Hyperparameter Tuning-----
# Tuning mtry (features sampled per split) and min_n (min samples per leaf).
# trees is fixed at 1000 — large enough for stable performance, not tuned
# (more trees is always at least as good; set high and stop).
#
# mtry upper bound set from the post-recipe predictor count, not raw train_data:
# dials defaults to mtry(range = c(1L, unknown())); unknown() is resolved via
# finalize(mtry(), train_data) which uses the RAW column count. But lr_recipe
# uses step_rm(), step_zv(), and step_corr(threshold = 0.90) — all drop predictors
# — so finalize() over-counts and some grid values would exceed the actual
# predictor count during CV, causing errors. We compute the exact post-recipe
# bound instead.
max_pred_bound <- ncol(bake(prep(lr_recipe), new_data = NULL)) - 1  # = 108

randfor_grid <-
  grid_regular(
    mtry(range = c(1L, max_pred_bound)),
    min_n(),
    levels = 5
  )

set.seed(12345)
the_folds <- vfold_cv(train_data, v = 5, strata = outcome)

# tune_grid returns a tibble where each row is a resample x hyperparam combo,
# with a .metrics list column
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)
randfor_tune <- tune_grid(randfor_workflow, resamples = the_folds, grid = randfor_grid)
stopCluster(cl)

# select_by_one_std_err isn't as justifiable here because there is no neat
# "simpler = better" axis: lower mtry adds randomization, higher min_n prunes,
# and these pull in different directions with no canonical regularization ordering.
randfor_favorite <- select_best(randfor_tune, metric = "roc_auc")
# this outputs a 1-row tibble of mtry, min_n, .config

show_best(randfor_tune, metric = "roc_auc")
autoplot(randfor_tune)


# Model Fit --------
randfor_final_wf <- finalize_workflow(randfor_workflow, randfor_favorite)

randfor_fit <- randfor_final_wf |> fit(data = train_data)


# CV Metrics --------
# Divergence: tune_grid fit on 5-fold subsets of train_data (each fold holds out
# 1/5 for validation). The final model below is a fresh fit on ALL train_data.
# mean = average of the 5 fold-level roc_auc .estimates tune_grid computed internally;
# std_err = sd of those estimates / sqrt(5); n = 5.
# sens/spec omitted here: not available from tune_grid without save_pred=TRUE.
randfor_cv_metrics <- collect_metrics(randfor_tune) |>
  dplyr::filter(
    .metric == "roc_auc",
    mtry    == randfor_favorite$mtry,
    min_n   == randfor_favorite$min_n
  ) |>
  dplyr::select(.metric, mean, std_err, n)
randfor_cv_metrics


# Train Metrics --------
# Divergence: randfor_fit was trained on ALL of train_data (not CV folds).
# relapse_pred_randfor is in-sample prediction on the same train_data — intentionally
# optimistic, equivalent to Balise et al.'s "Full Training Dataset" column.
relapse_pred_randfor <-
  predict(randfor_fit, train_data, type = "prob") |>
  bind_cols(train_data |> select(outcome))

# Sensitivity & Specificity at the Youden-J cutoff (chosen on TRAIN, not 0.5),
# matching logistic_enet.R so analysis.qmd reports a consistent operating point.
randfor_cut <- youden_cutoff(relapse_pred_randfor)
randfor_cut

randfor_train_metrics <- dplyr::bind_rows(
  roc_auc(relapse_pred_randfor, truth = outcome, .pred_1, event_level = "second"),
  sens_spec_at(relapse_pred_randfor, randfor_cut)
) |> dplyr::select(.metric, .estimate)
randfor_train_metrics


# Review Fit on Training Data --------
# Visualization of in-sample fit. relapse_pred_randfor and randfor_cut defined above.

# ROC/AUC Plot (train) — enriched with title/labels. # enriched 6/2
relapse_pred_randfor |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "Random Forest: training ROC curve",
    subtitle = "Relapse as the positive class; mtry and min_n selected by best CV AUC",
    x = "1 - Specificity", y = "Sensitivity"
  )

# ROC/AUC Score table
relapse_pred_randfor |>
  roc_curve(
    truth = outcome,
    .pred_1,
    event_level = "second"
  )


# Test Metrics --------
# Divergence: last_fit fits on the training split of data_split, evaluates on
# the held-out 25% test split. Youden-J cutoff chosen on train_data, applied here.

# last_fit() fits the final best model to the training set and evaluates the test set
randfor_last_fit <- randfor_final_wf |> last_fit(data_split)

# accuracy and roc_auc on the held-out test set
collect_metrics(randfor_last_fit)

# Test sens + spec at the SAME train-chosen cutoff (choose on train, report on
# test — never tuned on test). spec is the clinically important one here.
sens_spec_at(collect_predictions(randfor_last_fit), randfor_cut)

# Named metric tibble for the cross-method comparison table in analysis.qmd
randfor_test_metrics <- test_metrics_from_lastfit(
  randfor_last_fit, randfor_cut, "Random Forest"
)


# Review Fit on Test Data --------
# collect_predictions(randfor_last_fit) reuses the test-split predictions already
# computed inside last_fit() above — identical to predict(randfor_fit, test_data)
# but avoids a redundant prediction call and keeps this plot consistent with the
# scalar metrics derived from the same last_fit object.
relapse_pred_randfor_test <- collect_predictions(randfor_last_fit)

# Random forest ROC curves are smooth — many unique probability values from
# aggregated tree votes, unlike CART's staircase shape.
relapse_pred_randfor_test |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "Random Forest: test ROC curve",
    subtitle = "Held-out 25% test split; relapse as the positive class",
    x = "1 - Specificity", y = "Sensitivity"
  )

# Look at the results per person
randfor_person_predictions <- randfor_last_fit |> collect_predictions()

# Confusion Matrix
randfor_confusion_matrix <- randfor_person_predictions |> conf_mat(outcome, .pred_class)

# Prediction Probabilities — density of predicted P(relapse) by true outcome
collect_predictions(randfor_last_fit) |>
  ggplot2::ggplot(ggplot2::aes(x = .pred_1, fill = outcome)) +
  ggplot2::geom_density(alpha = 0.5) +
  ggplot2::labs(
    title = "True Outcome vs Prediction Probability",
    x     = "Probability of Relapse",
    y     = "Density"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = element_blank())

# Look at Variable Importance ------
# Random forest uses native impurity-based importance: for each variable, ranger
# accumulates the weighted Gini impurity decrease across every split where that
# variable was used, averaged across all 1000 trees. Averaged over many trees,
# this is more stable than CART's single-tree importance. Still favors
# high-cardinality continuous predictors; treat as indicative.
vip::vip(randfor_fit, num_features = 15, geom = "col") +
  ggplot2::labs(
    title    = "Random Forest variable importance",
    subtitle = "Native impurity-based importance (ranger engine, 1000 trees)",
    x        = "Importance (mean Gini decrease across trees)",
    y        = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11)
