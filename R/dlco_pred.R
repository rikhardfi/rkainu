#' Predicted DLCO values using Kainu et al. (2017) Finnish reference
#'
#' Computes predicted (mean) diffusing capacity values for Finnish adults using
#' the reference equations from Kainu et al. (2017).
#'
#' @param age Numeric vector. Age in years.
#' @param height Numeric vector. Height in cm. Values below 3 are assumed to be
#'   in metres and are automatically converted.
#' @param weight Numeric vector. Weight in kg.
#' @param sex Integer vector. 1 = male, 2 = female.
#' @param param Character vector of DLCO parameters. Valid values:
#'   `"DLCO"` (mmol/min/kPa), `"DLCOVA"` (mmol/min/kPa/L), `"VA"` (L).
#'
#' @return If a single parameter is requested, a numeric vector. If multiple
#'   parameters are requested, a data.frame with one column per parameter.
#'
#' @references
#' Kainu A, Timonen KL, Toikka J, et al. Reference values of diffusing
#' capacity — an 11-year follow-up of a national population sample.
#' *Clin Physiol Funct Imaging*. 2017;37(4):365-372.
#'
#' @examples
#' # Predicted DLCO for a 50-year-old male, 175 cm, 70 kg
#' dlco_pred(50, 175, 70, 1, "DLCO")
#'
#' # Multiple parameters
#' dlco_pred(50, 175, 70, 1, c("DLCO", "DLCOVA", "VA"))
#'
#' # Vectorized
#' dlco_pred(c(30, 50, 70), 175, 70, 1, "DLCO")
#'
#' @export
dlco_pred <- function(age, height, weight, sex = 1, param = "DLCO") {
  param <- .validate_dlco_params(param)
  v <- .validate_dlco_inputs(age, height, weight, sex)

  if (length(param) == 1L) {
    res <- .dlco_compute(v$age, v$height, v$weight, v$sex, param)
    return(res$predicted)
  }

  out <- lapply(param, function(p) {
    .dlco_compute(v$age, v$height, v$weight, v$sex, p)$predicted
  })
  names(out) <- param
  as.data.frame(out)
}
