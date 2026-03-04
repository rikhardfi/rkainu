#' Predicted spirometry values using Kainu et al. (2015) Finnish reference
#'
#' Computes predicted (mean) spirometry values for Finnish adults using the
#' reference equations from Kainu et al. (2015).
#'
#' @param age Numeric vector. Age in years.
#' @param height Numeric vector. Height in cm. Values below 3 are assumed to be
#'   in metres and are automatically converted.
#' @param sex Integer vector. 1 = male, 2 = female.
#' @param param Character vector of spirometry parameters. Valid values:
#'   `"FVC"`, `"FEV1"`, `"FEV1FVC"`, `"FEV6"`, `"FEV1FEV6"`, `"PEF"`,
#'   `"MMEF"`, `"MEF75"`, `"MEF50"`, `"MEF25"`.
#'
#' @return If a single parameter is requested, a numeric vector. If multiple
#'   parameters are requested, a data.frame with one column per parameter.
#'
#' @references
#' Kainu A, Timonen KL, Toikka J, et al. Reference values of spirometry for
#' Finnish adults. *Clin Physiol Funct Imaging*. 2016;36(5):346-358.
#'
#' @examples
#' # Predicted FEV1 for a 50-year-old male, 177 cm
#' kainu_pred(50, 177, 1, "FEV1")
#'
#' # Multiple parameters
#' kainu_pred(50, 177, 1, c("FEV1", "FVC", "FEV1FVC"))
#'
#' # Vectorized
#' kainu_pred(c(30, 50, 70), 170, 2, "FVC")
#'
#' @export
kainu_pred <- function(age, height, sex = 1, param = "FEV1") {
  param <- .validate_params(param)
  v <- .validate_inputs(age, height, sex)

  if (length(param) == 1L) {
    res <- .kainu_compute(v$age, v$height, v$sex, param)
    return(res$predicted)
  }

  out <- lapply(param, function(p) {
    .kainu_compute(v$age, v$height, v$sex, p)$predicted
  })
  names(out) <- param
  as.data.frame(out)
}
