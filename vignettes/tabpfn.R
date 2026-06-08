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
