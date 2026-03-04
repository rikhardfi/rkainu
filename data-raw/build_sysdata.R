# Build internal package data from Kainu et al. 2015 supplement
# Run this script from the package root: source("data-raw/build_sysdata.R")

library(readxl)

xlsx_path <- "inst/extdata/kainu_2015_supplement.xlsx"
sheets <- excel_sheets(xlsx_path)

# --- 1. Parse sheet names into param + sex ---
parse_sheet <- function(sheet) {
  sex <- if (grepl("female", tolower(sheet))) 2L else 1L
  param <- sub("^Kainu\\s+", "", sheet)
  param <- sub("\\s+(male|female)$", "", param, ignore.case = TRUE)
  param <- gsub("/", "", param)  # "FEV1/FVC" -> "FEV1FVC"
  list(param = param, sex = sex)
}

# --- 2. Extract coefficients ---
coeff_list <- lapply(sheets, function(s) {
  info <- parse_sheet(s)
  a0 <- as.numeric(read_excel(xlsx_path, sheet = s, range = "I4", col_names = FALSE, col_types = "numeric"))
  a1 <- as.numeric(read_excel(xlsx_path, sheet = s, range = "I5", col_names = FALSE, col_types = "numeric"))
  a2 <- as.numeric(read_excel(xlsx_path, sheet = s, range = "I6", col_names = FALSE, col_types = "numeric"))
  b0 <- as.numeric(read_excel(xlsx_path, sheet = s, range = "K4", col_names = FALSE, col_types = "numeric"))
  b1 <- as.numeric(read_excel(xlsx_path, sheet = s, range = "K6", col_names = FALSE, col_types = "numeric"))
  data.frame(
    param = info$param, sex = info$sex,
    a0 = a0, a1 = a1, a2 = a2, b0 = b0, b1 = b1,
    stringsAsFactors = FALSE
  )
})
kainu_coefficients <- do.call(rbind, coeff_list)
rownames(kainu_coefficients) <- NULL

cat("Coefficients extracted:\n")
print(kainu_coefficients)

# --- 3. Extract spline tables ---
spline_list <- lapply(sheets, function(s) {
  info <- parse_sheet(s)
  d <- read_excel(xlsx_path, sheet = s, range = cell_cols(1:4))
  names(d) <- tolower(names(d))
  # Standardize column names
  if ("spline" %in% names(d) && !"sspline" %in% names(d)) {
    names(d)[names(d) == "spline"] <- "sspline"
  }
  data.frame(
    param = info$param,
    sex = info$sex,
    age = d$age,
    mspline = d$mspline,
    sspline = d$sspline,
    stringsAsFactors = FALSE
  )
})
kainu_splines <- do.call(rbind, spline_list)
rownames(kainu_splines) <- NULL

cat("\nSplines: ", nrow(kainu_splines), "rows,",
    length(unique(kainu_splines$param)), "params,",
    "age range", min(kainu_splines$age), "-", max(kainu_splines$age), "\n")

# --- 4. Save as internal data ---
usethis::use_data(kainu_coefficients, kainu_splines, internal = TRUE, overwrite = TRUE)

cat("\nDone. R/sysdata.rda created.\n")
