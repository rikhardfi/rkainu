################################################################################
# Combined Kainu Coefficients, Adjustments, and Z-Score Calculation Script
#
# README / Instructions:
#
# This script imports Kainu et al. (2015) reference spirometry coefficients and
# adjustment values from their respective Excel supplements, then computes
# predicted values, SDs, and z-scores for FVC, FEV1, and FEV1/FVC in your
# spirometry dataset.
#
# Steps to use:
#
# 1. Install the required R packages if not already installed:
#      install.packages(c("readxl", "dplyr"))
#
# 2. Modify the two file-path variables below to point to your local copies of:
#      - The Kainu coefficients Excel file (each sheet contains coefficients for
#        a specific flow-volume variable and sex).
#      - The Kainu adjustments Excel file (each sheet contains MSpline/SSpline
#        adjustments for each flow-volume variable and sex).
#
#      Replace the placeholder strings ("path/to/...") with the actual file paths.
#
#      Example:
#        coefficients_file_path   <- "data/Kainu_Coefficients.xlsx"
#        adjustments_file_path    <- "data/Kainu_Adjustments.xlsx"
#
# 3. Ensure you have a data.frame named `spirodata` in your R environment, with
#    at least the following columns:
#      - age          (numeric; in full years)
#      - height       (numeric; in cm)
#      - sex          (character; "M" for male or "F" for female)
#      - FVC          (numeric; observed FVC value)
#      - FEV1         (numeric; observed FEV1 value)
#      - FEV1FVC      (numeric; observed FEV1/FVC ratio)
#
#    If your column names differ, either rename them accordingly or adjust the
#    arguments when calling `calc_z_scores()` below.
#
# 4. Source (or run) this script in your R session. This will:
#      a) Load the required libraries.
#      b) Read and combine all sheets from both Excel files into two data.frames:
#         `coefficients_dataset` and `Kainu_adjustments`.
#      c) Define helper functions:
#         - `calc_z_for_metric()`: computes predicted mean, SD, and z-score for a
#           single spirometric metric.
#         - `calc_z_scores()`: applies the above for FVC, FEV1, and FEV1/FVC across
#           your entire `spirodata` dataset.
#
# 5. After sourcing, run the example usage at the bottom (or adapt it as needed):
#
#      # Compute the z-scores:
#      df_with_z <- calc_z_scores(spirodata,
#                                 coefficients_dataset,
#                                 Kainu_adjustments)
#      View(df_with_z)
#
# NOTES:
# - The `sex` column in `spirodata` must be "M" or "F". Internally, this is
#   converted to numeric (0 = male, 1 = female) to match the coefficient data.
# - The `flow_volume_variable` names are parsed from sheet names (e.g.,
#   "Kainu FVC male" → "FVC").
# - If any MSpline or SSpline values are missing (NA), they default to 0.
# - You can rename or reformat columns in `df_with_z` as desired after running.
#
################################################################################

###############################################################################
# 2. Import Kainu coefficients and adjustments (robust, always lower-case)
################################################################################
library(readxl)
library(dplyr)

# coefficients_file_path on sourssattu jo aiemmin constants.R esim. "kainu/Kainu et al. 2015 supplement 1.xlsx"

coefficients_dataset <- lapply(excel_sheets(coefficients_file_path), function(sheet) {
  a0 <- read_excel(coefficients_file_path, sheet = sheet, range = "I4",
                   col_names = FALSE, col_types = "numeric") %>% as.numeric()
  a1 <- read_excel(coefficients_file_path, sheet = sheet, range = "I5",
                   col_names = FALSE, col_types = "numeric") %>% as.numeric()
  a2 <- read_excel(coefficients_file_path, sheet = sheet, range = "I6",
                   col_names = FALSE, col_types = "numeric") %>% as.numeric()
  b0 <- read_excel(coefficients_file_path, sheet = sheet, range = "K4",
                   col_names = FALSE, col_types = "numeric") %>% as.numeric()
  b1 <- read_excel(coefficients_file_path, sheet = sheet, range = "K6",
                   col_names = FALSE, col_types = "numeric") %>% as.numeric()
  sex <- ifelse(grepl("female", tolower(sheet)), 1,
                ifelse(grepl("male", tolower(sheet)), 0, NA))
  flow_volume_variable <- sub("Kainu (.*?) (male|female)", "\\1", sheet)
  data.frame(
    flow_volume_variable = flow_volume_variable,
    sex                  = sex,
    a0                   = a0,
    a1                   = a1,
    a2                   = a2,
    b0                   = b0,
    b1                   = b1,
    stringsAsFactors     = FALSE
  )
}) %>% bind_rows()

# Always lowercase all columns from adjustments
Kainu_adjustments <- lapply(excel_sheets(adjustments_file_path), function(sheet) {
  data <- read_excel(adjustments_file_path, sheet = sheet, range = cell_cols(1:4)) %>%
    rename_with(tolower)
  data$sex <- ifelse(grepl("female", tolower(sheet)), 1,
                     ifelse(grepl("male", tolower(sheet)), 0, NA))
  flow_volume_variable <- sub("Kainu (.*?) (male|female)", "\\1", sheet)
  data$flow_volume_variable <- flow_volume_variable
  data
}) %>% bind_rows()

################################################################################
# 3. Z-score calculation helpers (ALWAYS uses mspline/sspline in lowercase)
################################################################################
calc_z_for_metric <- function(data, metric, observed_col, coeffs, adjustments) {
  data_metric <- data %>%
    left_join(
      filter(coeffs, flow_volume_variable == metric),
      by = c("sex", "flow_volume_variable")
    ) %>%
    left_join(
      filter(adjustments, flow_volume_variable == metric),
      by = c("age", "sex", "flow_volume_variable")
    ) %>%
    mutate(
      mspline = if_else(is.na(mspline), 0, mspline),
      sspline = if_else(is.na(sspline), 0, sspline)
    ) %>%
    mutate(
      pred    = exp(a0 + a1 * log(height) + a2 * log(age) + mspline),
      sd_val  = exp(b0 + b1 * log(age) + sspline),
      z_score = ( .data[[observed_col]] - pred ) / sd_val
    ) %>%
    rename(
      !!paste0("pred_", metric) := pred,
      !!paste0("sd_",   metric) := sd_val,
      !!paste0("z_",    metric) := z_score
    ) %>%
    select(
      -a0, -a1, -a2, -b0, -b1,
      -mspline, -sspline
    )
}

calc_z_scores <- function(spirodata, coeffs_dataset, adjustments_dataset) {
  spiro <- spirodata
  spiro_FVC      <- spiro %>% mutate(flow_volume_variable = "FVC")
  spiro_FEV1     <- spiro %>% mutate(flow_volume_variable = "FEV1")
  spiro_FEV1FVC  <- spiro %>% mutate(flow_volume_variable = "FEV1FVC")
  result_FVC     <- calc_z_for_metric(spiro_FVC,     "FVC",     "FVC",     coeffs_dataset, adjustments_dataset)
  result_FEV1    <- calc_z_for_metric(spiro_FEV1,    "FEV1",    "FEV1",    coeffs_dataset, adjustments_dataset)
  result_FEV1FVC <- calc_z_for_metric(spiro_FEV1FVC, "FEV1FVC", "FEV1FVC", coeffs_dataset, adjustments_dataset)
  spiro <- spiro %>% mutate(row_number = row_number())
  combined <- spiro %>%
    left_join(
      result_FVC     %>% mutate(row_number = row_number()) %>%
        select(row_number, starts_with("pred_FVC"), starts_with("sd_FVC"), starts_with("z_FVC")),
      by = "row_number"
    ) %>%
    left_join(
      result_FEV1    %>% mutate(row_number = row_number()) %>%
        select(row_number, starts_with("pred_FEV1"), starts_with("sd_FEV1"), starts_with("z_FEV1")),
      by = "row_number"
    ) %>%
    left_join(
      result_FEV1FVC %>% mutate(row_number = row_number()) %>%
        select(row_number, starts_with("pred_FEV1FVC"), starts_with("sd_FEV1FVC"), starts_with("z_FEV1FVC")),
      by = "row_number"
    ) %>%
    select(-row_number)
  return(combined)
}

################################################################################
# 4. Example usage
################################################################################
# After running everything above:
# df_results <- calc_z_scores(df, coefficients_dataset, Kainu_adjustments)
# View(df_results)

Kainu_adjustments <- Kainu_adjustments %>%
  rename(sspline = spline)