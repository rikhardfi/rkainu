#' rkainu: Finnish Spirometry and DLCO Reference Values
#'
#' Predicted values, z-scores, lower limits of normal (LLN), and percent
#' predicted for Finnish adult spirometry (Kainu et al. 2015) and diffusing
#' capacity (DLCO; Kainu et al. 2017).
#'
#' @section Spirometry functions:
#' \describe{
#'   \item{[kainu_pred()]}{Predicted (mean) values}
#'   \item{[kainu_lln()]}{Lower limit of normal (5th percentile)}
#'   \item{[kainu_zscore()]}{Z-scores from observed values}
#'   \item{[kainu_pctpred()]}{Percent predicted from observed values}
#'   \item{[kainu_df()]}{Data.frame convenience wrapper}
#' }
#'
#' @section DLCO functions:
#' \describe{
#'   \item{[dlco_pred()]}{Predicted DLCO, DLCO/VA, or VA}
#'   \item{[dlco_lln()]}{Lower limit of normal for DLCO parameters}
#'   \item{[dlco_zscore()]}{Z-scores from observed DLCO values}
#' }
#'
#' @references
#' Kainu A, Timonen KL, Toikka J, et al. Reference values of spirometry for
#' Finnish adults. \emph{Clin Physiol Funct Imaging}. 2016;36(5):346-358.
#' \doi{10.1111/cpf.12237}
#'
#' Kainu A, Timonen KL, Toikka J, et al. Reference values of diffusing
#' capacity for Finnish adults. \emph{Clin Physiol Funct Imaging}.
#' 2018;38(3):413-420. \doi{10.1111/cpf.12432}
#'
#' @keywords internal
"_PACKAGE"
