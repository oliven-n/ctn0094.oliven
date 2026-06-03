# metrics_helpers.R
# Shared classification-metric helpers used across the model scripts
# (logistic_enet.R, knn.R, knn_wopv.R). They read sens/spec at a chosen operating
# cutoff instead of the hardwired 0.5 in predict(type = "class"), so every model
# reports sens/spec at a comparable operating point in analysis.qmd.
#
# Outcome convention: factor with levels c(0, 1); relapse = "1" = second level =
# positive class. Predictions must carry a `.pred_1` column and an `outcome` column.
#
# Sourcing only DEFINES these functions; yardstick/dplyr need only be loaded by the
# time they are CALLED (they are, after the model scripts source their data setup).

# Youden-J optimal probability cutoff from a tibble with (.pred_1, outcome).
youden_cutoff <- function(preds) {
  preds |>
    roc_curve(truth = outcome, .pred_1, event_level = "second") |>
    dplyr::mutate(youden_j = sensitivity + specificity - 1) |>
    dplyr::slice_max(youden_j, n = 1, with_ties = FALSE) |>
    dplyr::pull(.threshold)
}

# sens + spec at an arbitrary probability cutoff (relapse = "1" = second level).
sens_spec_at <- function(preds, cutoff) {
  labeled <- preds |>
    dplyr::mutate(.pred_cut = factor(dplyr::if_else(.pred_1 >= cutoff, "1", "0"),
                                     levels = c("0", "1")))
  dplyr::bind_rows(
    sens(labeled, truth = outcome, estimate = .pred_cut, event_level = "second"),
    spec(labeled, truth = outcome, estimate = .pred_cut, event_level = "second")
  )
}

# Test-set metric tibble (accuracy, roc_auc, sens, spec) from a last_fit object,
# with sens/spec read at a TRAIN-chosen Youden cutoff. Returns columns
# method/.metric/.estimate so all model scripts can be row-bound in analysis.qmd.
# added 6/2
test_metrics_from_lastfit <- function(last_fit_obj, cutoff, method_label) {
  preds <- tune::collect_predictions(last_fit_obj)
  base  <- tune::collect_metrics(last_fit_obj) |>     # accuracy + roc_auc
    dplyr::select(.metric, .estimate)
  ss <- sens_spec_at(preds, cutoff) |>                # sens + spec at cutoff
    dplyr::select(.metric, .estimate)
  dplyr::bind_rows(base, ss) |>
    dplyr::mutate(method = method_label) |>
    dplyr::select(method, .metric, .estimate)
}
