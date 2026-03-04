#' Lower limit of normal (LLN) using Kainu et al. (2015) Finnish reference
#'
#' Computes the 5th percentile lower limit of normal for Finnish adults.
#' `LLN = predicted - 1.645 * SD`.
#'
#' @inheritParams kainu_pred
#'
#' @return If a single parameter is requested, a numeric vector. If multiple
#'   parameters are requested, a data.frame with one column per parameter.
#'
#' @references
#' Kainu A, Timonen KL, Toikka J, et al. Reference values of spirometry for
#' Finnish adults. *Clin Physiol Funct Imaging*. 2016;36(5):346-358.
#'
#' @examples
#' # LLN for FEV1, 50-year-old male, 177 cm
#' kainu_lln(50, 177, 1, "FEV1")
#'
#' @export
kainu_lln <- function(age, height, sex = 1, param = "FEV1") {
  param <- .validate_params(param)
  v <- .validate_inputs(age, height, sex)

  if (length(param) == 1L) {
    res <- .kainu_compute(v$age, v$height, v$sex, param)
    return(res$lln)
  }

  out <- lapply(param, function(p) {
    .kainu_compute(v$age, v$height, v$sex, p)$lln
  })
  names(out) <- param
  as.data.frame(out)
}
