#' Compute Kainu reference values for a data.frame
#'
#' Convenience wrapper that adds predicted, LLN, z-score, and/or percent
#' predicted columns to an existing data.frame.
#'
#' @param data A data.frame containing age, height, sex, and observed
#'   spirometry columns.
#' @param age_col,height_col,sex_col Character strings naming the columns for
#'   age (years), height (cm), and sex (1=male, 2=female).
#' @param params Character vector of spirometry parameters to compute. Observed
#'   values are read from columns matching these names (e.g., `"FEV1"`).
#' @param output Character vector specifying which outputs to add. Any
#'   combination of `"predicted"`, `"lln"`, `"zscore"`, `"pctpred"`. Defaults
#'   to all four.
#'
#' @return The input data.frame with added columns named
#'   `pred_<param>`, `lln_<param>`, `z_<param>`, `pctpred_<param>`.
#'   Z-score and percent predicted columns are only added for parameters that
#'   exist as columns in the input data.
#'
#' @references
#' Kainu A, Timonen KL, Toikka J, et al. Reference values of spirometry for
#' Finnish adults. *Clin Physiol Funct Imaging*. 2016;36(5):346-358.
#'
#' @examples
#' df <- data.frame(
#'   age = c(30, 50, 70),
#'   height = c(180, 175, 170),
#'   sex = c(1, 1, 2),
#'   FEV1 = c(4.5, 3.8, 2.0),
#'   FVC = c(5.5, 4.9, 2.8)
#' )
#' kainu_df(df)
#'
#' @export
kainu_df <- function(data, age_col = "age", height_col = "height",
                     sex_col = "sex",
                     params = c("FEV1", "FVC", "FEV1FVC"),
                     output = c("predicted", "lln", "zscore", "pctpred")) {
  params <- .validate_params(params)
  output <- match.arg(output, c("predicted", "lln", "zscore", "pctpred"),
                      several.ok = TRUE)

  age <- data[[age_col]]
  height <- data[[height_col]]
  sex <- data[[sex_col]]

  if (is.null(age) || is.null(height) || is.null(sex)) {
    stop("Columns '", age_col, "', '", height_col, "', '", sex_col,
         "' must exist in the data.")
  }

  v <- .validate_inputs(age, height, sex)

  for (p in params) {
    res <- .kainu_compute(v$age, v$height, v$sex, p)

    if ("predicted" %in% output) {
      data[[paste0("pred_", p)]] <- res$predicted
    }
    if ("lln" %in% output) {
      data[[paste0("lln_", p)]] <- res$lln
    }
    if ("zscore" %in% output && p %in% names(data)) {
      obs <- data[[p]]
      data[[paste0("z_", p)]] <- (obs - res$predicted) / res$sd
    }
    if ("pctpred" %in% output && p %in% names(data)) {
      obs <- data[[p]]
      data[[paste0("pctpred_", p)]] <- (obs / res$predicted) * 100
    }
  }

  data
}
