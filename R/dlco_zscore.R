#' Z-scores for DLCO using Kainu et al. (2017) Finnish reference
#'
#' Computes z-scores for observed diffusing capacity values. Supply observed
#' values as named arguments; only the supplied parameters are computed.
#'
#' @param age Numeric vector. Age in years.
#' @param height Numeric vector. Height in cm. Values below 3 are assumed to be
#'   in metres and are automatically converted.
#' @param weight Numeric vector. Weight in kg.
#' @param sex Integer vector. 1 = male, 2 = female.
#' @param DLCO Numeric vector of observed DLCO values (mmol/min/kPa).
#' @param DLCOVA Numeric vector of observed DLCO/VA values (mmol/min/kPa/L).
#' @param VA Numeric vector of observed alveolar volume values (L).
#'
#' @return If a single parameter is supplied, a numeric vector. If multiple
#'   parameters are supplied, a data.frame with one column per parameter.
#'
#' @references
#' Kainu A, Timonen KL, Toikka J, et al. Reference values of diffusing
#' capacity — an 11-year follow-up of a national population sample.
#' *Clin Physiol Funct Imaging*. 2017;37(4):365-372.
#'
#' @examples
#' # Z-score for observed DLCO = 10.0 mmol/min/kPa
#' dlco_zscore(50, 175, 70, 1, DLCO = 10.0)
#'
#' # Multiple parameters
#' dlco_zscore(50, 175, 70, 1, DLCO = 10.0, VA = 6.5)
#'
#' @export
dlco_zscore <- function(age, height, weight, sex = 1,
                        DLCO = NULL, DLCOVA = NULL, VA = NULL) {
  v <- .validate_dlco_inputs(age, height, weight, sex)

  # Collect supplied parameters
  supplied <- list(DLCO = DLCO, DLCOVA = DLCOVA, VA = VA)
  supplied <- supplied[!vapply(supplied, is.null, logical(1))]

  if (length(supplied) == 0L) {
    stop("At least one observed DLCO parameter must be supplied.")
  }

  results <- lapply(names(supplied), function(p) {
    .validate_dlco_params(p)
    obs <- supplied[[p]]
    res <- .dlco_compute(v$age, v$height, v$weight, v$sex, p)
    (obs - res$predicted) / res$sd
  })
  names(results) <- names(supplied)

  if (length(results) == 1L) return(results[[1L]])
  as.data.frame(results)
}
