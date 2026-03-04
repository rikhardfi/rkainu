#' Percent predicted using Kainu et al. (2015) Finnish reference
#'
#' Computes `(observed / predicted) * 100` for observed spirometry values.
#' Supply observed values as named arguments.
#'
#' @inheritParams kainu_zscore
#'
#' @return If a single parameter is supplied, a numeric vector. If multiple
#'   parameters are supplied, a data.frame with one column per parameter.
#'
#' @references
#' Kainu A, Timonen KL, Toikka J, et al. Reference values of spirometry for
#' Finnish adults. *Clin Physiol Funct Imaging*. 2016;36(5):346-358.
#'
#' @examples
#' kainu_pctpred(50, 177, 1, FEV1 = 4.0)
#'
#' @export
kainu_pctpred <- function(age, height, sex = 1,
                          FEV1 = NULL, FVC = NULL, FEV1FVC = NULL,
                          FEV6 = NULL, FEV1FEV6 = NULL, PEF = NULL,
                          MMEF = NULL, MEF75 = NULL, MEF50 = NULL,
                          MEF25 = NULL) {
  v <- .validate_inputs(age, height, sex)

  supplied <- list(
    FEV1 = FEV1, FVC = FVC, FEV1FVC = FEV1FVC, FEV6 = FEV6,
    FEV1FEV6 = FEV1FEV6, PEF = PEF, MMEF = MMEF,
    MEF75 = MEF75, MEF50 = MEF50, MEF25 = MEF25
  )
  supplied <- supplied[!vapply(supplied, is.null, logical(1))]

  if (length(supplied) == 0L) {
    stop("At least one observed spirometry parameter must be supplied.")
  }

  results <- lapply(names(supplied), function(p) {
    .validate_params(p)
    obs <- supplied[[p]]
    res <- .kainu_compute(v$age, v$height, v$sex, p)
    (obs / res$predicted) * 100
  })
  names(results) <- names(supplied)

  if (length(results) == 1L) return(results[[1L]])
  as.data.frame(results)
}
