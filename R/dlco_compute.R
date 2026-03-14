# Internal computation for DLCO reference equations

#' Validate and preprocess DLCO inputs
#' @return Named list with age, height_cm, weight_kg, sex — all same length.
#' @noRd
.validate_dlco_inputs <- function(age, height, weight, sex) {
  # Recycle length-1 inputs

n <- max(length(age), length(height), length(weight), length(sex))
  if (length(age) == 1L) age <- rep(age, n)
  if (length(height) == 1L) height <- rep(height, n)
  if (length(weight) == 1L) weight <- rep(weight, n)
  if (length(sex) == 1L) sex <- rep(sex, n)

  if (length(age) != n || length(height) != n ||
      length(weight) != n || length(sex) != n) {
    stop("age, height, weight, and sex must have the same length (or length 1).")
  }

  # Sex validation
  if (any(!sex %in% c(1L, 2L))) {
    stop("sex must be 1 (male) or 2 (female).")
  }

  # Reject negative/zero weight
  if (any(weight <= 0, na.rm = TRUE)) stop("Weight must be positive.")
  if (any(height < 0, na.rm = TRUE)) stop("Height must be positive.")
  if (any(age < 0, na.rm = TRUE)) stop("Age must be positive.")

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
  if (any(age < 18 | age > 83, na.rm = TRUE)) {
    warning("Kainu et al. (2017) DLCO reference values are for ages 18-83. ",
            "Values outside this range are extrapolated.")
  }

  list(age = as.numeric(age), height = as.numeric(height),
       weight = as.numeric(weight), sex = as.integer(sex))
}

#' Validate DLCO parameter names
#' @noRd
.validate_dlco_params <- function(param) {
  param <- toupper(param)
  bad <- setdiff(param, .DLCO_VALID_PARAMS)
  if (length(bad) > 0L) {
    stop("Unknown DLCO parameter(s): ", paste(bad, collapse = ", "),
         ". Valid: ", paste(.DLCO_VALID_PARAMS, collapse = ", "))
  }
  param
}

#' Core DLCO computation: predicted, sd, lln for one parameter
#'
#' @param age,height,weight,sex Validated numeric vectors (same length).
#'   Height in cm, weight in kg.
#' @param param Single parameter name ("DLCO", "DLCOVA", or "VA").
#' @return data.frame with columns: predicted, sd, lln.
#' @noRd
.dlco_compute <- function(age, height, weight, sex, param) {
  n <- length(age)
  predicted <- numeric(n)
  lln_val <- numeric(n)

  height_m <- height / 100

  for (sx in c(1L, 2L)) {
    mask <- sex == sx
    if (!any(mask)) next

    sx_key <- as.character(sx)
    co_mean <- .dlco_coefficients[[param]]$mean[[sx_key]]
    co_lln  <- .dlco_coefficients[[param]]$lln[[sx_key]]

    if (is.null(co_mean) || is.null(co_lln)) {
      stop("No DLCO coefficients for param=", param, " sex=", sx)
    }

    # f = exp(a0 + a1*age + a2*ln(age) + a3*(1/height_m) + a4*weight_kg)
    predicted[mask] <- exp(
      co_mean["a0"] +
      co_mean["a1"] * age[mask] +
      co_mean["a2"] * log(age[mask]) +
      co_mean["a3"] * (1 / height_m[mask]) +
      co_mean["a4"] * weight[mask]
    )

    lln_val[mask] <- exp(
      co_lln["a0"] +
      co_lln["a1"] * age[mask] +
      co_lln["a2"] * log(age[mask]) +
      co_lln["a3"] * (1 / height_m[mask]) +
      co_lln["a4"] * weight[mask]
    )
  }

  sd_val <- (predicted - lln_val) / 1.645

  data.frame(predicted = predicted, sd = sd_val, lln = lln_val)
}
