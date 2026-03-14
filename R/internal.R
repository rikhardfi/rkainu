# Internal (unexported) functions for rkainu

# Valid parameter names
.VALID_PARAMS <- c("FVC", "FEV1", "FEV1FVC", "FEV6", "FEV1FEV6",
                   "PEF", "MMEF", "MEF75", "MEF50", "MEF25")

#' Validate and preprocess inputs
#' @return Named list with age, height (cm), sex (1/2) — all same length.
#' @noRd
.validate_inputs <- function(age, height, sex) {
  # Recycle length-1 inputs
  n <- max(length(age), length(height), length(sex))
  if (length(age) == 1L) age <- rep(age, n)
  if (length(height) == 1L) height <- rep(height, n)
  if (length(sex) == 1L) sex <- rep(sex, n)

  if (length(age) != n || length(height) != n || length(sex) != n) {
    stop("age, height, and sex must have the same length (or length 1).")
  }

  # Reject negative values
  if (any(height < 0, na.rm = TRUE)) stop("Height must be positive.")
  if (any(age < 0, na.rm = TRUE)) stop("Age must be positive.")

  # Sex validation
  if (any(!sex %in% c(1L, 2L))) {
    stop("sex must be 1 (male) or 2 (female).")
  }

  # Height auto-detect: if all values < 3, assume metres

  if (all(height < 3, na.rm = TRUE)) {
    message("Height values appear to be in metres. Converting to cm.")
    height <- height * 100
  }

  # Height sanity check
  if (any(height < 100 | height > 220, na.rm = TRUE)) {
    warning("Some height values are outside 100-220 cm. Check units.")
  }

  # Age warning for out-of-range
  if (any(age < 18 | age > 90, na.rm = TRUE)) {
    warning("Kainu et al. (2015) reference values are for ages 18-90. ",
            "Values outside this range are extrapolated.")
  }

  list(age = as.numeric(age), height = as.numeric(height), sex = as.integer(sex))
}

#' Validate parameter names
#' @noRd
.validate_params <- function(param) {
  param <- toupper(param)
  bad <- setdiff(param, .VALID_PARAMS)
  if (length(bad) > 0L) {
    stop("Unknown parameter(s): ", paste(bad, collapse = ", "),
         ". Valid: ", paste(.VALID_PARAMS, collapse = ", "))
  }
  param
}

#' Look up spline adjustments with linear interpolation and clamping
#'
#' @param param Single parameter name (e.g., "FEV1").
#' @param sex Single sex value (1 or 2).
#' @param age Numeric vector of ages.
#' @return data.frame with columns mspline, sspline (same length as age).
#' @importFrom stats approx
#' @noRd
.get_spline <- function(param, sex, age) {
  # Subset spline table for this param/sex
  idx <- kainu_splines$param == param & kainu_splines$sex == sex
  sp <- kainu_splines[idx, , drop = FALSE]

  if (nrow(sp) == 0L) {
    stop("No spline data for param=", param, " sex=", sex)
  }

  sp_age <- sp$age
  age_min <- min(sp_age)
  age_max <- max(sp_age)

  # Clamp ages to table bounds
  age_clamped <- pmin(pmax(age, age_min), age_max)

  # Linear interpolation
  ms <- stats::approx(sp_age, sp$mspline, xout = age_clamped, rule = 2)$y
  ss <- stats::approx(sp_age, sp$sspline, xout = age_clamped, rule = 2)$y

  data.frame(mspline = ms, sspline = ss)
}

#' Core computation: predicted, sd, lln for one parameter
#'
#' @param age,height,sex Validated numeric vectors (same length).
#' @param param Single parameter name.
#' @return data.frame with columns: predicted, sd, lln.
#' @noRd
.kainu_compute <- function(age, height, sex, param) {
  n <- length(age)
  predicted <- numeric(n)
  sd_val <- numeric(n)

  # Process males and females separately (different coefficients + splines)
  for (sx in c(1L, 2L)) {
    mask <- sex == sx
    if (!any(mask)) next

    # Look up coefficients
    cidx <- kainu_coefficients$param == param & kainu_coefficients$sex == sx
    co <- kainu_coefficients[cidx, , drop = FALSE]
    if (nrow(co) == 0L) stop("No coefficients for param=", param, " sex=", sx)

    # Spline adjustments
    sp <- .get_spline(param, sx, age[mask])

    # Predicted mean: M = exp(a0 + a1*log(height) + a2*log(age) + mspline)
    predicted[mask] <- exp(co$a0 + co$a1 * log(height[mask]) +
                             co$a2 * log(age[mask]) + sp$mspline)

    # Predicted SD: S = exp(b0 + b1*log(age) + sspline)
    sd_val[mask] <- exp(co$b0 + co$b1 * log(age[mask]) + sp$sspline)
  }

  lln <- predicted - 1.645 * sd_val

  data.frame(predicted = predicted, sd = sd_val, lln = lln)
}
