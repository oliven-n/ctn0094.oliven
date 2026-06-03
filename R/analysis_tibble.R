#' Pre-treatment feature matrix for CTN-0094 relapse prediction
#'
#' One row per care-seeking adult with opioid use disorder from three harmonized
#' NIDA Clinical Trials Network studies (CTN-0027, CTN-0030, CTN-0051), conducted
#' 2006-2016. Columns are features engineered from the 28 days before
#' randomization, plus the modeled relapse outcome.
#'
#' @format A tibble with 2,492 rows and 116 columns. Key columns:
#' \describe{
#'   \item{who}{Participant identifier (role: ID; not a predictor).}
#'   \item{project}{Source study: CTN-0027, CTN-0030, or CTN-0051.}
#'   \item{age}{Age in years at randomization.}
#'   \item{is_male}{1 = male, 0 = female.}
#'   \item{withdrawal_pre_score}{Pre-induction withdrawal severity (0 None - 3 Severe).}
#'   \item{outcome}{Relapse during treatment: factor, levels c("0","1"); "1" = relapsed.}
#' }
#' Remaining columns are engineered pre-treatment predictors spanning substance
#' use (per-drug days, binary use, longest streak), demographics, addiction
#' severity, psychiatric comorbidity, pain, nicotine dependence, IV-use risk
#' behavior, and cross-domain interactions. See \code{vignettes/building_analysis.R}.
#'
#' @source Derived from the \pkg{public.ctn0094data} and \pkg{CTNote} packages via
#'   \code{data-raw/analysis_tibble.R}.
"analysis_tibble"
