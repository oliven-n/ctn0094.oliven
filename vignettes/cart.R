library(rpart)
library(doParallel)
if (!exists("train_data")) {
  source(here::here("vignettes/logistic_regression.R"))
}
source(here::here("vignettes/metrics_helpers.R"))


# Workflow ----------------
# CART uses the same shared recipe as the other models (lr_recipe).
cart_recipe <- lr_recipe

# i used parsnip_addin() - gotta run it in console - to get this sample code block
cart_spec <-
  # commented out tree_depth = tune(), min_n = tune(), cost_complexity = tune() from below
  decision_tree(cost_complexity=tune()) |>
  # the order of the below two doesnt matter and tbh im not sure why
  set_engine('rpart') |>
  set_mode('classification')


# can't pipe over the workflow because we have funn special thingz like cart_spec
cart_workflow <-
  workflow() |>
  add_recipe(cart_recipe) |>
  add_model(cart_spec)

# Hyperparameter Tuning-----
# (in our case there is just the one for now)
cart_grid <-
  grid_regular(cost_complexity(), levels=10)

set.seed(12345)
the_folds <- vfold_cv(train_data, v = 5, strata = outcome)

# tune_grid returns a tibble where each row is a resample x hyperparam combo,
# with a .metrics list column
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)
cart_tune <- tune_grid(cart_workflow, resamples = the_folds, grid = cart_grid)
stopCluster(cl)

#select_by_one_std_err to avoid overfitting, desc to prefer the simpler model within 1SE
favorite <- select_by_one_std_err(cart_tune, desc(cost_complexity), metric = "roc_auc")
# this outputs a 1x2 tibble of cost_complexity,.config

# An above plot sort of whowing the above process for favorite, but this would be
# if we used select_best() and then that would be the peak on show_best().
show_best(cart_tune, metric="roc_auc")
autoplot(cart_tune)


# Model Fit --------
final_wf <- finalize_workflow(cart_workflow, favorite)

cart_fit <- final_wf |> fit(data=train_data)


# CV Metrics --------
# Divergence: tune_grid fit on 5-fold subsets of train_data (each fold holds out
# 1/5 for validation). The final model below is a fresh fit on ALL train_data.
# mean = average of the 5 fold-level roc_auc .estimates tune_grid computed internally;
# std_err = sd of those estimates / sqrt(5); n = 5.
# sens/spec omitted here: not available from tune_grid without save_pred=TRUE.
cart_cv_metrics <- collect_metrics(cart_tune) |>
  dplyr::filter(.metric == "roc_auc", cost_complexity == favorite$cost_complexity) |>
  dplyr::select(.metric, mean, std_err, n)
cart_cv_metrics


# Train Metrics --------
# Divergence: cart_fit was trained on ALL of train_data (not CV folds).
# relapse_pred_cart is in-sample prediction on the same train_data — intentionally
# optimistic, equivalent to Balise et al.'s "Full Training Dataset" column.
relapse_pred_cart <-
  predict(cart_fit, train_data, type = "prob") |>
  bind_cols(train_data |> select(outcome))

# Sensitivity & Specificity at the Youden-J cutoff (chosen on TRAIN, not 0.5),
# matching logistic_enet.R so analysis.qmd reports a consistent operating point.
cart_cut <- youden_cutoff(relapse_pred_cart)
cart_cut

cart_train_metrics <- dplyr::bind_rows(
  roc_auc(relapse_pred_cart, truth = outcome, .pred_1, event_level = "second"),
  sens_spec_at(relapse_pred_cart, cart_cut)
) |> dplyr::select(.metric, .estimate)
cart_train_metrics


# Review Fit on Training Data --------
# Visualization of in-sample fit. relapse_pred_cart and cart_cut defined above.

# ROC/AUC Plot (train) — enriched with title/labels. # enriched 6/2
relapse_pred_cart |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "CART: training ROC curve",
    subtitle = "Relapse as the positive class; cost_complexity selected by 1-SE rule",
    x = "1 - Specificity", y = "Sensitivity"
  )

# ROC/AUC Score table
relapse_pred_cart |>
  roc_curve(
    truth = outcome,
    .pred_1,
    event_level = "second"
  )


# Test Metrics --------
# Divergence: last_fit fits on the training split of data_split, evaluates on
# the held-out 25% test split. Youden-J cutoff chosen on train_data, applied here.

# last_fit() fits the final best model to the training set and evaluates the test set
cart_last_fit <- final_wf |> last_fit(data_split)

# accuracy and roc_auc on test set (tidymodels defaults)
collect_metrics(cart_last_fit)

# Test sens + spec at the SAME train-chosen cutoff (choose on train, report on test)
sens_spec_at(collect_predictions(cart_last_fit), cart_cut)

# Named metric tibble for the cross-method comparison table in analysis.qmd
cart_test_metrics <- test_metrics_from_lastfit(
  cart_last_fit, cart_cut, "CART"
)


# Review Fit on Test Data --------
# collect_predictions(cart_last_fit) reuses the test-split predictions already
# computed inside last_fit() above — identical to predict(cart_fit, test_data)
# but avoids a redundant prediction call and keeps this plot consistent with the
# scalar metrics derived from the same last_fit object.
relapse_pred_cart_test <- collect_predictions(cart_last_fit)

# CART ROC curves are staircase-shaped — terminal nodes produce a small number of
# unique probability values (one per leaf), so the curve has discrete jumps
# rather than the smooth arc seen in logistic/enet models.
relapse_pred_cart_test |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  ggplot2::labs(
    title    = "CART: test ROC curve",
    subtitle = "Held-out 25% test split; relapse as the positive class",
    x = "1 - Specificity", y = "Sensitivity"
  )

# Looking at the tree
library(rpart.plot)

cart_last_fit |>
  extract_fit_engine() |>
  rpart.plot(roundint=FALSE)

# Look at the results per person
cart_person_predictions <- cart_last_fit |> collect_predictions()

# Confusion Matrix
cart_confusion_matrix <- cart_person_predictions |> conf_mat(outcome, .pred_class)

# Prediction Probabilities — density of predicted P(relapse) by true outcome
collect_predictions(cart_last_fit) |>
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
# On CART, we use native impurity-based importance (mean decrease in node impurity
# weighted by the number of samples reaching each split) rather than permutation
# importance. This is cheap to compute — it is read directly from the fitted rpart
# object — and is the conventional importance for tree models.
# NOTE: impurity importance can favor high-cardinality continuous predictors; treat
# the ranking as indicative rather than precise.
vip::vip(cart_fit, num_features = 20) +
  ggplot2::labs(
    title    = "CART variable importance",
    subtitle = "Native impurity-based importance from the fitted rpart tree",
    x        = "Importance (mean node impurity decrease)",
    y        = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11)
