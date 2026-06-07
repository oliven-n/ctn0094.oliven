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
    trees          = 5000,     # upper bound only — early stopping exits well before this
    learn_rate     = tune(),
    tree_depth     = tune(),
    loss_reduction = tune(),
    mtry           = tune(),   # colsample_bytree: fraction of predictors per tree
    sample_size    = tune(),   # subsample: fraction of training rows per tree
    stop_iter      = 50        # stop if no AUC improvement for 50 consecutive rounds
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
# With 5 params, grid_regular explodes (3^5 = 243 combos); use a space-filling
# design instead — covers the hyperparameter volume efficiently in ~30 runs.
#
# Initial range reasoning:
#   learn_rate    small  c(-3, -1): 0.001–0.1 — slow shrinkage, less overfit
#   tree_depth    low    c(1, 5):   shallow trees, controls per-tree complexity
#   loss_reduction —     c(-2, 1):  log10 scale → gamma 0.01–10; includes near-zero
#                                   (xgboost default) through moderate pruning
#   mtry          —      c(30, 90):  integer cols sampled per tree (30–90 of ~104)
#   sample_size   —      c(0.5,0.9): subsample proportion (50–90% of rows/tree)
#
# After tuning, run the console diagnostic plot (see repo notes) to assess
# whether any axis needs widening/tightening before the final grid.
# could use grid_regular but it would have every grid point in the hypercube
# here we get to pick the size to be coarser
xgboost_grid <- grid_space_filling(
  learn_rate(range     = c(-7, -1.5)),
  tree_depth(range     = c(2L, 5L)),
  loss_reduction(range = c(-7, 1)),
  mtry(range           = c(10L, 90L)),
  sample_prop(range    = c(0.5, 0.9)),
  size = 200
)

set.seed(12345)
the_folds <- vfold_cv(train_data, v = 5, strata = outcome)

# tune_grid returns a tibble where each row is a resample x hyperparam combo,
# with a .metrics list column
xgboost_cache <- here::here("vignettes/xgboost_tune.rds")
if (file.exists(xgboost_cache)) {
  xgboost_tune <- readRDS(xgboost_cache)
} else {
  cl <- makePSOCKcluster(parallel::detectCores() - 1)
  registerDoParallel(cl)
  xgboost_tune <- tune_grid(
    xgboost_workflow,
    resamples = the_folds,
    grid      = xgboost_grid,
    control   = control_grid(
      # new-xgboost (2.x/3.x) stores the early-stopping pick under
      # attributes(...)$early_stop$best_iteration; the old $best_iteration is gone.
      extract   = \(x) tibble::tibble(best_iter = attributes(extract_fit_engine(x))$early_stop$best_iteration)
    )
  )
  stopCluster(cl)
  saveRDS(xgboost_tune, xgboost_cache)
}

# How many trees did each grid point actually use before early stopping?
# Read this TABLE alongside show_best (AUC) — tree count alone doesn't decide:
#   - best_iter pinned near the 5000 cap = still improving when cut off
#     (budget-limited, not converged). The CV AUC tune_grid reports for these
#     is REAL and if anything a slight UNDERestimate (the model was still
#     climbing) — so a capped point is still fully valid to use and select as
#     best. "More trees" is upside left on the table, not a validity problem.
#     Just pin trees = 5000 at finalize time (last_fit has no early-stop set).
#   - best_iter well below the cap = converged on its own. Then check its AUC:
#     good AUC -> low learn_rate works, keep exploring down; weak AUC -> ramp up.
xgboost_tune |>
  select(id, .extracts) |>
  tidyr::unnest(.extracts) |>
  tidyr::unnest(.extracts) |>
  group_by(learn_rate, tree_depth, loss_reduction, mtry, sample_size) |>
  summarise(mean_trees = mean(best_iter), .groups = "drop") |>
  arrange(mean_trees)

# Trees-to-converge vs learn_rate (the visual version of the table above).
# x = log10(learn_rate); y = mean best_iter over folds. The dashed line is the
# 5000-tree cap: points riding it at low learn_rate are budget-limited; points
# sitting well below it converged on their own.
xgboost_tune |>
  select(id, .extracts) |>
  tidyr::unnest(.extracts) |>
  tidyr::unnest(.extracts) |>
  group_by(learn_rate) |>
  summarise(mean_trees = mean(best_iter), .groups = "drop") |>
  ggplot2::ggplot(ggplot2::aes(x = log10(learn_rate), y = mean_trees)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 5000, linetype = "dashed") +
  ggplot2::labs(
    title = "Trees to converge vs learn_rate",
    x     = "log10(learn_rate)",
    y     = "trees before early stopping (mean over folds)"
  )
# IMPORTANT: FROM HERE WE ARE SEEING NEARLY EVERY SINGLE POINT OS STILL CONVERGING
# AT THE 5,000 TREE CAP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# THIS DOESN'T INVALIDATE OUR ROCAUC, JUST SHOULD KNOW THAT WHAT THE HYPERPARAMS
# BECOME IS BEING LEFT ENTIRELY UP TO SHOW_BEST() WHICH IS ARBITRARY ONCE WE ARE
# INSIDE THAT NEAR-ZERO RANGER!

# run in console and re-adjust window/tune_grid to hone in on desired roc_auc
xgboost_tune |>
  collect_metrics() |>
  dplyr::filter(.metric == "roc_auc") |>
  dplyr::select(mean, learn_rate, tree_depth, loss_reduction, mtry, sample_size) |>
  tidyr::pivot_longer(-mean, values_to = "value", names_to = "parameter") |>
  ggplot2::ggplot(ggplot2::aes(value, mean, color = parameter)) +
  ggplot2::geom_point(show.legend = FALSE) +
  ggplot2::facet_wrap(~parameter, scales = "free_x") +
  ggplot2::labs(x = NULL, y = "AUC")

show_best(xgboost_tune, metric = "roc_auc")

# select_by_one_std_err isn't as justifiable here because there is no neat
# "simpler = better" axis: smaller learn_rate shrinks more aggressively,
# shallower tree_depth constrains complexity — these pull in different directions
# with no canonical regularization ordering like CART's single cost_complexity.
xgboost_favorite <- select_best(xgboost_tune, metric = "roc_auc")
# this outputs a 1-row tibble of learn_rate, tree_depth, loss_reduction, mtry, sample_size, .config

show_best(xgboost_tune, metric = "roc_auc")
#autoplot(xgboost_tune)


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
    subtitle = "Native gain-based importance (xgboost engine, 5000 trees)",
    x        = "Importance (mean gain across splits)",
    y        = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11)
