# Z-score calculation
library(dplyr)

#' Calculate Spirometry z-scores based on Kainu et al. (2015)
#'
#' This function adds z-score variables for FVC, FEV1, and FEV1/FVC to the input data frame.
#' It uses reference equations with coefficients from Kainu et al. (2015). The equations are:
#'
#' For each parameter, the predicted mean (M) is calculated as:
#'   M = exp(a0 + a1 * log(height) + a2 * log(age))
#'
#' and the predicted standard deviation (SD) as:
#'   SD = exp(b0 + b1 * log(age))
#'
#' (Spline contributions are assumed to be 0.)
#'
#' The z-score is then:
#'   z = (observed - M) / SD
#'
#' Coefficients differ by sex. If no "sex" column is present, males ("M") are assumed.
#'
#' @param df A data frame containing the following columns:
#'   - age: Age in years
#'   - height: Height in cm
#'   - FVC: Forced vital capacity in liters
#'   - FEV1: Forced expiratory volume in 1 second in liters
#'   - FEV1FVC: The FEV1/FVC ratio (as a decimal, e.g. 0.75)
#'   - (optional) sex: "M" or "F" indicating sex. If missing, "M" is assumed.
#'
#' @return The original data frame with additional columns:
#'   - z_FVC, z_FEV1, z_FEV1FVC
#'
#' @references
#' Kainu, A. et al. (2015). Reference values of spirometry for Finnish adults.
#' See also the official R documentation for [log()] and [exp()].
#' dplyr documentation: <https://dplyr.tidyverse.org>
calc_z_scores <- function(df) {
  
  # If 'sex' column is missing, assume male ("M")
  if (!"sex" %in% names(df)) {
    df$sex <- "M"
  }
  
  df <- df %>%
    mutate(
      # Predicted FVC and its SD
      pred_FVC = if_else(toupper(sex) == "M",
                         exp(-8.903 + 2.165 * log(height) - 0.173 * log(age)),
                         exp(-9.278 + 2.214 * log(height) - 0.188 * log(age))),
      sd_FVC = if_else(toupper(sex) == "M",
                       exp(-1.652 + 0.319 * log(age)),
                       exp(-0.219 - 0.149 * log(age))),
      z_FVC = (FVC - pred_FVC) / sd_FVC,
      
      # Predicted FEV1 and its SD
      pred_FEV1 = if_else(toupper(sex) == "M",
                          exp(-7.437 + 1.908 * log(height) - 0.275 * log(age)),
                          exp(-7.205 + 1.858 * log(height) - 0.316 * log(age))),
      sd_FEV1 = if_else(toupper(sex) == "M",
                        exp(-1.547 + 0.239 * log(age)),
                        exp(-0.032 + 0.266 * log(age))),
      z_FEV1 = (FEV1 - pred_FEV1) / sd_FEV1,
      
      # Predicted FEV1/FVC and its SD
      pred_FEV1FVC = if_else(toupper(sex) == "M",
                             exp(1.673 - 0.292 * log(height) - 0.108 * log(age)),
                             exp(1.869 - 0.311 * log(height) - 0.135 * log(age))),
      sd_FEV1FVC = if_else(toupper(sex) == "M",
                           exp(-2.643 - 0.065 * log(age)),
                           exp(-3.076 + 0.023 * log(age))),
      z_FEV1FVC = (FEV1FVC - pred_FEV1FVC) / sd_FEV1FVC
    )
  
  return(df)
}

# Example usage:
# Suppose your spirodata is in a data frame called spirodata.
# It must contain columns: age, height, FVC, FEV1, FEV1FVC, and optionally sex.
#
# spirodata <- read.csv("your_spirodata.csv")
spirodata <- calc_z_scores(spirodata)
# View(spirodata)