library(reticulate)
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
    dplyr::select(-dplyr::any_of(c("who", "outcome"))) |>
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

tabpfn_fit <- tabpfn$TabPFNClassifier(device = "cpu", ignore_pretraining_limits = TRUE)  # ~1,869 rows exceeds the 1,000-sample CPU default; flag suppresses the RuntimeError
tabpfn_fit$fit(X_train, y_train)

# predict_proba columns follow the sorted classes_ ; resolve the relapse ("1")
# column by position so we never hard-code an index.
relapse_col <- which(as.integer(tabpfn_fit$classes_) == 1L)
stopifnot(length(relapse_col) == 1L)  # guard: classes_ must contain exactly one "1" class
