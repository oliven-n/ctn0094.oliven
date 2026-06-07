library(xgboost)
library(doParallel)
if (!exists("train_data")) {
  source(here::here("vignettes/logistic_regression.R"))
}
source(here::here("vignettes/metrics_helpers.R"))


# Workflow ----------------
# XGBoost uses the same shared recipe as the other models (lr_recipe).
xgboost_recipe <- lr_recipe

xgboost_spec <-
  boost_tree(
    trees          = 1000,
    learn_rate     = tune(),
    tree_depth     = tune(),
    loss_reduction = tune(),
    mtry           = tune(),   # colsample_bytree: fraction of predictors per tree
    sample_size    = tune()    # subsample: fraction of training rows per tree
  ) |>
  set_engine('xgboost') |>
  set_mode('classification')

# can't pipe over the workflow because we have funn special thingz like xgboost_spec
xgboost_workflow <-
  workflow() |>
  add_recipe(xgboost_recipe) |>
  add_model(xgboost_spec)

# Hyperparameter Tuning-----
# Tuning 5 parameters; trees fixed at 1000 (fixing avoids a 6th dimension and
# 1000 rounds at a small learn_rate is a solid conservative default).
# With 5 params, grid_regular explodes (3^5 = 243 combos); use a Latin hypercube
# instead — space-filling design that covers the hyperparameter volume in ~30 runs.
#
# Initial range reasoning:
#   learn_rate    small  c(-3, -1): 0.001–0.1 — slow shrinkage, less overfit
#   tree_depth    low    c(1, 5):   shallow trees, controls per-tree complexity
#   loss_reduction high  c(0, 2):   log10 scale → gamma 1–100; requires splits
#                                   to earn their keep (conservative pruning)
#   mtry          —      c(0.3,0.9): colsample_bytree proportion (30–90% of cols)
#   sample_size   —      c(0.5,0.9): subsample proportion (50–90% of rows/tree)
#
# After tuning, run the console diagnostic plot (see repo notes) to assess
# whether any axis needs widening/tightening before the final grid.
xgboost_grid <- grid_latin_hypercube(
  learn_rate(range     = c(-3, -1)),
  tree_depth(range     = c(1L, 5L)),
  loss_reduction(range = c(0, 2)),
  mtry_prop(range      = c(0.3, 0.9)),
  sample_prop(range    = c(0.5, 0.9)),
  size = 30
)

set.seed(12345)
the_folds <- vfold_cv(train_data, v = 5, strata = outcome)

# tune_grid returns a tibble where each row is a resample x hyperparam combo,
# with a .metrics list column
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)
xgboost_tune <- tune_grid(xgboost_workflow, resamples = the_folds, grid = xgboost_grid)
stopCluster(cl)

# select_by_one_std_err isn't as justifiable here because there is no neat
# "simpler = better" axis: smaller learn_rate shrinks more aggressively,
# shallower tree_depth constrains complexity — these pull in different directions
# with no canonical regularization ordering like CART's single cost_complexity.
xgboost_favorite <- select_best(xgboost_tune, metric = "roc_auc")
# this outputs a 1-row tibble of learn_rate, tree_depth, loss_reduction, mtry, sample_size, .config

show_best(xgboost_tune, metric = "roc_auc")
autoplot(xgboost_tune)


# Model Fit --------
xgboost_final_wf <- finalize_workflow(xgboost_workflow, xgboost_favorite)

xgboost_fit <- xgboost_final_wf |> fit(data = train_data)


# CV Metrics --------
# Divergence: tune_grid fit on 5-fold subsets of train_data (each fold holds out
# 1/5 for validation). The final model below is a fresh fit on ALL train_data.
# mean = average of the 5 fold-level roc_auc .estimates tune_grid computed internally;
# std_err = sd of those estimates / sqrt(5); n = 5.
# sens/spec omitted here: not available from tune_grid without save_pred=TRUE.
xgboost_cv_metrics <- collect_metrics(xgboost_tune) |>
  dplyr::filter(
    .metric        == "roc_auc",
    learn_rate     == xgboost_favorite$learn_rate,
    tree_depth     == xgboost_favorite$tree_depth,
    loss_reduction == xgboost_favorite$loss_reduction,
    mtry           == xgboost_favorite$mtry,
    sample_size    == xgboost_favorite$sample_size
  ) |>
  dplyr::select(.metric, mean, std_err, n)
xgboost_cv_metrics


# Train Metrics --------
# Divergence: xgboost_fit was trained on ALL of train_data (not CV folds).
# relapse_pred_xgboost is in-sample prediction on the same train_data — intentionally
# optimistic, equivalent to Balise et al.'s "Full Training Dataset" column.
relapse_pred_xgboost <-
  predict(xgboost_fit, train_data, type = "prob") |>
  bind_cols(train_data |> select(outcome))

# Sensitivity & Specificity at the Youden-J cutoff (chosen on TRAIN, not 0.5),
# matching logistic_enet.R so analysis.qmd reports a consistent operating point.
xgboost_cut <- youden_cutoff(relapse_pred_xgboost)
xgboost_cut

xgboost_train_metrics <- dplyr::bind_rows(
  roc_auc(relapse_pred_xgboost, truth = outcome, .pred_1, event_level = "second"),
  sens_spec_at(relapse_pred_xgboost, xgboost_cut)
) |> dplyr::select(.metric, .estimate)
xgboost_train_metrics


# Review Fit on Training Data --------
# Visualization of in-sample fit. relapse_pred_xgboost and xgboost_cut defined above.

# ROC/AUC Plot (train) — enriched with title/labels. # enriched 6/2
relapse_pred_xgboost |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "XGBoost: training ROC curve",
    subtitle = "Relapse as the positive class; learn_rate and tree_depth selected by best CV AUC",
    x = "1 - Specificity", y = "Sensitivity"
  )

# ROC/AUC Score table
relapse_pred_xgboost |>
  roc_curve(
    truth = outcome,
    .pred_1,
    event_level = "second"
  )


# Test Metrics --------
# Divergence: last_fit fits on the training split of data_split, evaluates on
# the held-out 25% test split. Youden-J cutoff chosen on train_data, applied here.

# last_fit() fits the final best model to the training set and evaluates the test set
xgboost_last_fit <- xgboost_final_wf |> last_fit(data_split)

# accuracy and roc_auc on the held-out test set
collect_metrics(xgboost_last_fit)

# Test sens + spec at the SAME train-chosen cutoff (choose on train, report on
# test — never tuned on test). spec is the clinically important one here.
sens_spec_at(collect_predictions(xgboost_last_fit), xgboost_cut)

# Named metric tibble for the cross-method comparison table in analysis.qmd
xgboost_test_metrics <- test_metrics_from_lastfit(
  xgboost_last_fit, xgboost_cut, "XGBoost"
)


# Review Fit on Test Data --------
# collect_predictions(xgboost_last_fit) reuses the test-split predictions already
# computed inside last_fit() above — identical to predict(xgboost_fit, test_data)
# but avoids a redundant prediction call and keeps this plot consistent with the
# scalar metrics derived from the same last_fit object.
relapse_pred_xgboost_test <- collect_predictions(xgboost_last_fit)

relapse_pred_xgboost_test |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "XGBoost: test ROC curve",
    subtitle = "Held-out 25% test split; relapse as the positive class",
    x = "1 - Specificity", y = "Sensitivity"
  )

# Look at the results per person
xgboost_person_predictions <- xgboost_last_fit |> collect_predictions()

# Confusion Matrix
xgboost_confusion_matrix <- xgboost_person_predictions |> conf_mat(outcome, .pred_class)

# Prediction Probabilities — density of predicted P(relapse) by true outcome
collect_predictions(xgboost_last_fit) |>
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
# XGBoost provides native gain-based importance: for each variable, the average
# improvement in prediction accuracy (gain) across all splits where that variable
# was used. Unlike impurity importance, gain weights splits by their actual
# contribution to accuracy, not just split frequency.
vip::vip(xgboost_fit, num_features = 15, geom = "col") +
  ggplot2::labs(
    title    = "XGBoost variable importance",
    subtitle = "Native gain-based importance (xgboost engine, 1000 trees)",
    x        = "Importance (mean gain across splits)",
    y        = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11)
