#' Lower limit of normal (LLN) for DLCO using Kainu et al. (2017)
#'
#' Computes the 5th percentile lower limit of normal for diffusing capacity
#' in Finnish adults.
#'
#' @inheritParams dlco_pred
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
#' # LLN for DLCO, 50-year-old male, 175 cm, 70 kg
#' dlco_lln(50, 175, 70, 1, "DLCO")
#'
#' @export
dlco_lln <- function(age, height, weight, sex = 1, param = "DLCO") {
  param <- .validate_dlco_params(param)
  v <- .validate_dlco_inputs(age, height, weight, sex)

  if (length(param) == 1L) {
    res <- .dlco_compute(v$age, v$height, v$weight, v$sex, param)
    return(res$lln)
  }

  out <- lapply(param, function(p) {
    .dlco_compute(v$age, v$height, v$weight, v$sex, p)$lln
  })
  names(out) <- param
  as.data.frame(out)
}
