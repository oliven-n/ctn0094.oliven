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

#
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
