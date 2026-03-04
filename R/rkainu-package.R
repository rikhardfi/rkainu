#' rkainu: Finnish Spirometry Reference Values
#'
#' Predicted values, z-scores, lower limits of normal (LLN), and percent
#' predicted for Finnish adult spirometry using the Kainu et al. (2015)
#' reference equations.
#'
#' The package covers 10 spirometry parameters: FVC, FEV1, FEV1/FVC, FEV6,
#' FEV1/FEV6, PEF, MMEF, MEF75, MEF50, and MEF25, for adults aged 18-90.
#'
#' @section Main functions:
#' \describe{
#'   \item{[kainu_pred()]}{Predicted (mean) values}
#'   \item{[kainu_lln()]}{Lower limit of normal (5th percentile)}
#'   \item{[kainu_zscore()]}{Z-scores from observed values}
#'   \item{[kainu_pctpred()]}{Percent predicted from observed values}
#'   \item{[kainu_df()]}{Data.frame convenience wrapper}
#' }
#'
#' @references
#' Kainu A, Timonen KL, Toikka J, et al. Reference values of spirometry for
#' Finnish adults. *Clin Physiol Funct Imaging*. 2016;36(5):346-358.
#'
#' @keywords internal
"_PACKAGE"
