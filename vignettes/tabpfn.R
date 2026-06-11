library(reticulate)
library(tidymodels)
library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)
library(vip)
if (!exists("train_data")) {
  source(here::here("vignettes/logistic_regression.R"))
}
source(here::here("vignettes/metrics_helpers.R"))

# Point reticulate at the TabPFN env (created via virtualenv_create("tabpfn-env")).
use_virtualenv("tabpfn-env", required = TRUE)
tabpfn <- import("tabpfn")

# Workflow ----------------
# TabPFN has no parsnip/tidymodels engine, so there is NO model spec or workflow
# object here (unlike knn.R). We reuse the SAME shared recipe and drive the Python
# model directly via reticulate.
tabpfn_recipe <- lr_recipe

# Helpers: the recipe one-hot encodes + normalizes everything, so a baked frame is
# fully numeric apart from `who` (role "ID") and `outcome` (response). bake_X drops
# those two and returns a numeric matrix; y01 pulls the 0/1 outcome as an integer.
bake_X <- function(prepped, new) {
  bake(prepped, new_data = new) |>
    select(-any_of(c("who", "outcome"))) |>
    as.matrix()
}
y01 <- function(df) as.integer(as.character(df$outcome))

# Model Fit --------
# "Fitting" TabPFN stores the training set as in-context memory for the pretrained
# transformer (no weights are learned). We prep the recipe on ALL of train_data and
# fit on the full baked matrix — the analog of knn's final fit on all train_data.
tabpfn_prep <- prep(tabpfn_recipe, training = train_data)
X_train <- bake_X(tabpfn_prep, train_data)
y_train <- y01(train_data)

# The RHS of below is declaring an instance of the TabPFNClassifier object using reticulate's tabpfn
# the LHS is naming that fit object, R style,
tabpfn_fit <- tabpfn$TabPFNClassifier(device = "cpu", ignore_pretraining_limits = TRUE)  # ~1,869 rows exceeds the 1,000-sample CPU default; flag suppresses the RuntimeError
# The TabPFNClassifier class has hyperparams like device, but not *tuning* hyperparams
# because we are just doing one forward pass with the already pre-trained transformer weights

# The below calls the .fit() method, but using R syntax of classinst$method rather than python's classinst.method()
# this literally ISN't a fit in the usual sense of learning parameters! tabpfn doesnt do that
# fit() here *just* stores the training data (X_train, y_train)

# When u try to inspect this method it wont tell u it called the method directly.
#you cant know fit() didnt work by direct inspection:
#either something will break in the method call or relapse_col and predict_proba() will break later
tabpfn_fit$fit(X_train, y_train)

# predict_proba columns follow the sorted classes_ ; resolve the relapse ("1")
# column by position so we never hard-code an index.
relapse_col <- which(as.integer(tabpfn_fit$classes_) == 1L)
stopifnot(length(relapse_col) == 1L)  # guard: classes_ must contain exactly one "1" class

# Inspect classes_ and relapse_col to verify column resolution
as.integer(tabpfn_fit$classes_)           # expect: 0 1
relapse_col  # expect: 2, and yes

# predict_proba() is like predict(), and relies on the training data stored by fit()
# as context. For each test point, the pretrained transformer runs one forward pass
# seeing both the training examples and that test point simultaneously to output a probability.

#this is just an inspect,though we did run it technically but arent saving to var.
# [1:3, ] only shows first 3 preds
# we have to do this because not saving to a variable force prints all 1868 train rows
# this is leave one out, so all other 1867 rows are used as context for each row.
# this (or rather our actual call that we save to a var later) is to get the "all train"
# performance col in the reporting table (6, currently) in analysis qmd
tabpfn_fit$predict_proba(X_train)[1:3, ] # expect: 3x2 matrix, rows sum to ~1
# works! we are good to go.

# CV Metrics --------
# Divergence from knn.R: TabPFN has no tune_grid/collect_metrics. We reproduce
# tune_grid's leakage-safe behavior by hand — for each of 5 folds, prep the recipe
# on the ANALYSIS rows only, bake the held-out ASSESSMENT rows, fit TabPFN on the
# analysis matrix, and collect out-of-fold .pred_1. roc_auc per fold then
# mean/std_err/n mirror knn_cv_metrics' columns (mean = average of 5 fold AUCs,
# std_err = sd/sqrt(5), n = 5). TabPFN inference is fast, so this runs serially.
set.seed(12345)
the_folds <- vfold_cv(train_data, v = 5, strata = outcome)

# normally, prep and bake would be called internally inside the workflow by tune_grid(), fit(), and last_fit()
# but because TabPFN has no parsnip engine, there is no model spec and therefore no workflow, so we call them manually here per fold

tabpfn_cv_cache <- here::here("vignettes/tabpfn_cv.rds")
if (file.exists(tabpfn_cv_cache)) {
  tabpfn_cv_auc <- readRDS(tabpfn_cv_cache)
} else {
  tabpfn_cv_auc <- map_dfr(the_folds$splits, function(split) {
    tr <- analysis(split)
    va <- assessment(split)
    prepped <- prep(tabpfn_recipe, training = tr)        # fit recipe on analysis fold only
    clf <- tabpfn$TabPFNClassifier(device = "cpu", ignore_pretraining_limits = TRUE)
    clf$fit(bake_X(prepped, tr), y01(tr))
    col <- which(as.integer(clf$classes_) == 1L)
    preds <- tibble(
      .pred_1 = clf$predict_proba(bake_X(prepped, va))[, col],
      outcome = va$outcome
    )
    roc_auc(preds, truth = outcome, .pred_1, event_level = "second")
  })
  saveRDS(tabpfn_cv_auc, tabpfn_cv_cache)
}

tabpfn_cv_metrics <- tibble(
  .metric = "roc_auc",
  mean    = mean(tabpfn_cv_auc$.estimate),
  std_err = sd(tabpfn_cv_auc$.estimate) / sqrt(nrow(tabpfn_cv_auc)),
  n       = nrow(tabpfn_cv_auc)
)
tabpfn_cv_metrics

# Train Metrics --------
# Divergence: tabpfn_fit was "fit" on ALL of train_data; relapse_pred_tabpfn is
# in-sample prediction on the same train_data — intentionally optimistic,
# equivalent to Balise et al.'s "Full Training Dataset" column.
# Uses leave-one-out context: each training row predicted using all others as context.
proba_train <- tabpfn_fit$predict_proba(X_train)
relapse_pred_tabpfn <- tibble(
  .pred_0 = proba_train[, setdiff(1:2, relapse_col)],
  .pred_1 = proba_train[, relapse_col],
  outcome = train_data$outcome
)

tabpfn_cut <- youden_cutoff(relapse_pred_tabpfn)
tabpfn_cut

tabpfn_train_metrics <- bind_rows(
  roc_auc(relapse_pred_tabpfn, truth = outcome, .pred_1, event_level = "second"),
  sens_spec_at(relapse_pred_tabpfn, tabpfn_cut)
) |> select(.metric, .estimate)
tabpfn_train_metrics

# Review Fit on Training Data --------
# Visualization of in-sample fit. relapse_pred_tabpfn and tabpfn_cut defined above.

# ROC/AUC Plot (train) — enriched with title/labels.
relapse_pred_tabpfn |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  labs(
    title    = "TabPFN: training ROC curve",
    subtitle = "Relapse as the positive class; pretrained transformer (no tuning)",
    x = "1 - Specificity", y = "Sensitivity"
  )

# ROC/AUC Score table
relapse_pred_tabpfn |>
  roc_curve(
    truth = outcome,
    .pred_1,
    event_level = "second"
  )

# Test Metrics --------
# No last_fit object → build the method/.metric/.estimate tibble by hand to match
# the helper's schema so analysis.qmd still row-binds it.
# Structure verified against knn_test_metrics: same column names/order/types
# and same .metric order: accuracy, roc_auc, sens, spec.
# Youden-J cutoff chosen on train_data (tabpfn_cut), applied here.
X_test <- bake_X(tabpfn_prep, test_data)
proba_test <- tabpfn_fit$predict_proba(X_test)
relapse_pred_tabpfn_test <- tibble(
  .pred_0 = proba_test[, setdiff(1:2, relapse_col)],
  .pred_1 = proba_test[, relapse_col],
  outcome = test_data$outcome
)

# accuracy at the tidymodels default 0.5 threshold (matches collect_metrics()),
# then roc_auc, then sens/spec at the TRAIN-chosen cutoff — same order
# test_metrics_from_lastfit() emits (collect_metrics' accuracy+roc_auc, then ss).
acc_at_half <- relapse_pred_tabpfn_test |>
  mutate(.pred_class = factor(if_else(.pred_1 >= 0.5, "1", "0"),
                              levels = c("0", "1"))) |>
  accuracy(truth = outcome, estimate = .pred_class)

tabpfn_test_metrics <- bind_rows(
  acc_at_half,
  roc_auc(relapse_pred_tabpfn_test, truth = outcome, .pred_1, event_level = "second"),
  sens_spec_at(relapse_pred_tabpfn_test, tabpfn_cut)
) |>
  transmute(method = "TabPFN", .metric, .estimate)

# Structural contract check: tabpfn_test_metrics MUST match knn_test_metrics'
# schema so analysis.qmd's bind_rows of the per-method tables stays valid.
if (exists("knn_test_metrics")) {
  stopifnot(
    identical(names(tabpfn_test_metrics), names(knn_test_metrics)),
    identical(sapply(tabpfn_test_metrics, class), sapply(knn_test_metrics, class)),
    identical(tabpfn_test_metrics$.metric, knn_test_metrics$.metric)
  )
}
tabpfn_test_metrics

# Review Fit on Test Data --------
# relapse_pred_tabpfn_test holds the held-out test-split predictions computed above.
relapse_pred_tabpfn_test |>
  roc_curve(truth = outcome, .pred_1, event_level = "second") |>
  autoplot() +
  labs(
    title    = "TabPFN: test ROC curve",
    subtitle = "Held-out 25% test split; relapse as the positive class",
    x = "1 - Specificity", y = "Sensitivity"
  )

# Look at Variable Importance ------
# TabPFN has no intrinsic coefficients, so (like knn.R) we use model-agnostic
# PERMUTATION importance: shuffle one predictor at a time and measure the drop in
# test ROC AUC. A bigger drop = a more important predictor.

# AUC metric honoring the positive class ("1" = relapse = second level).
# vip requires the metric fn to take arguments named `truth` and `estimate`.
tabpfn_auc_metric <- function(truth, estimate) {
  roc_auc_vec(truth = truth, estimate = estimate, event_level = "second")
}


tabpfn_vip_cache <- here::here("vignettes/tabpfn_vip.rds")
if (file.exists(tabpfn_vip_cache)) {
  tabpfn_vip_obj <- readRDS(tabpfn_vip_cache)
} else {
  set.seed(12345)
  n_passes <- length(setdiff(names(train_data), c("who", "outcome"))) * 1L
  pass_count <- 0L
  tabpfn_vip_obj <- vi_permute(
    object        = tabpfn_fit,
    feature_names = setdiff(names(train_data), c("who", "outcome")),
    train         = train_data,
    target        = "outcome",
    metric        = tabpfn_auc_metric,
    smaller_is_better = FALSE,
    pred_wrapper  = function(object, newdata) {
      # vip subsets newdata to feature_names, dropping the `who` ID column the
      # recipe needs; re-add a dummy (who has role "ID", so it never affects preds).
      if (!"who" %in% names(newdata)) newdata$who <- 1L
      # TabPFN needs a baked numeric matrix — bake through tabpfn_prep before predicting.
      proba <- tabpfn_fit$predict_proba(bake_X(tabpfn_prep, newdata))
      pass_count <<- pass_count + 1L
      cat(sprintf("\r%d / %d passes complete", pass_count, n_passes))
      flush.console()
      proba[, relapse_col]
    },
    nsim     = 1
  )
  saveRDS(tabpfn_vip_obj, tabpfn_vip_cache)
}

# VIP plot (top 15), enriched with title/axis/description.
vip(tabpfn_vip_obj, num_features = 15, geom = "col") +
  labs(
    title    = "TabPFN permutation variable importance",
    subtitle = "Mean drop in test ROC AUC when each predictor is shuffled (1 permutation)",
    x        = "Importance (mean ROC AUC decrease)",
    y        = NULL
  ) +
  theme_minimal(base_size = 11)
