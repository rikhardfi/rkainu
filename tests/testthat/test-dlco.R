# ---- Validation tests against known values ----

test_that("DLCO pred matches validation (50yr male 175cm 70.44kg)", {
  res <- dlco_pred(50, 175, 70.44, 1, "DLCO")
  expect_equal(round(res, 2), 10.59)
})

test_that("DLCOVA pred matches validation (50yr male 175cm 70.44kg)", {
  res <- dlco_pred(50, 175, 70.44, 1, "DLCOVA")
  expect_equal(round(res, 2), 1.53)
})

test_that("DLCO pred matches validation (50yr female 165cm 62.64kg)", {
  res <- dlco_pred(50, 165, 62.64, 2, "DLCO")
  expect_equal(round(res, 2), 7.53)
})

test_that("DLCOVA pred matches validation (50yr female 165cm 62.64kg)", {
  res <- dlco_pred(50, 165, 62.64, 2, "DLCOVA")
  expect_equal(round(res, 2), 1.46)
})

# ---- dlco_pred basic behaviour ----

test_that("dlco_pred returns numeric for single param", {
  res <- dlco_pred(50, 175, 70, 1, "DLCO")
  expect_type(res, "double")
  expect_length(res, 1)
})

test_that("dlco_pred returns data.frame for multiple params", {
  res <- dlco_pred(50, 175, 70, 1, c("DLCO", "DLCOVA", "VA"))
  expect_s3_class(res, "data.frame")
  expect_equal(ncol(res), 3)
  expect_equal(names(res), c("DLCO", "DLCOVA", "VA"))
})

test_that("dlco_pred is vectorized over age", {
  res <- dlco_pred(c(30, 50, 70), 175, 70, 1, "DLCO")
  expect_length(res, 3)
  expect_true(all(diff(res) < 0))
})

test_that("dlco_pred is vectorized over height", {
  res <- dlco_pred(50, c(165, 175, 185), 70, 1, "DLCO")
  expect_length(res, 3)
  expect_true(all(diff(res) > 0))
})

test_that("dlco_pred male > female at same size", {
  m <- dlco_pred(50, 175, 70, 1, "DLCO")
  f <- dlco_pred(50, 175, 70, 2, "DLCO")
  expect_true(m > f)
})

# ---- dlco_lln basic behaviour ----

test_that("dlco_lln returns numeric for single param", {
  res <- dlco_lln(50, 175, 70, 1, "DLCO")
  expect_type(res, "double")
  expect_length(res, 1)
})

test_that("dlco_lln < dlco_pred", {
  pred <- dlco_pred(50, 175, 70, 1, "DLCO")
  lln  <- dlco_lln(50, 175, 70, 1, "DLCO")
  expect_true(lln < pred)
})

test_that("dlco_lln returns data.frame for multiple params", {
  res <- dlco_lln(50, 175, 70, 1, c("DLCO", "VA"))
  expect_s3_class(res, "data.frame")
  expect_equal(ncol(res), 2)
})

# ---- dlco_zscore basic behaviour ----

test_that("dlco_zscore returns numeric for single param", {
  res <- dlco_zscore(50, 175, 70, 1, DLCO = 10.0)
  expect_type(res, "double")
  expect_length(res, 1)
})

test_that("dlco_zscore returns data.frame for multiple params", {
  res <- dlco_zscore(50, 175, 70, 1, DLCO = 10.0, VA = 6.0)
  expect_s3_class(res, "data.frame")
  expect_equal(ncol(res), 2)
})

test_that("dlco_zscore of predicted value is ~0", {
  pred <- dlco_pred(50, 175, 70, 1, "DLCO")
  z <- dlco_zscore(50, 175, 70, 1, DLCO = pred)
  expect_equal(z, 0, tolerance = 1e-10)
})

test_that("dlco_zscore of LLN is ~-1.645", {
  lln <- dlco_lln(50, 175, 70, 1, "DLCO")
  z <- dlco_zscore(50, 175, 70, 1, DLCO = lln)
  expect_equal(z, -1.645, tolerance = 1e-6)
})

test_that("dlco_zscore errors when no param supplied", {
  expect_error(dlco_zscore(50, 175, 70, 1), "At least one")
})

# ---- Input validation ----

test_that("height in metres is auto-detected and converted", {
  expect_message(
    res <- dlco_pred(50, 1.75, 70, 1, "DLCO"),
    "metres"
  )
  res_cm <- suppressMessages(dlco_pred(50, 175, 70, 1, "DLCO"))
  expect_equal(res, res_cm)
})

test_that("invalid sex produces error", {
  expect_error(dlco_pred(50, 175, 70, 3, "DLCO"), "1 \\(male\\) or 2 \\(female\\)")
})

test_that("age outside 18-83 produces warning", {
  expect_warning(dlco_pred(15, 175, 70, 1, "DLCO"), "18-83")
  expect_warning(dlco_pred(90, 175, 70, 1, "DLCO"), "18-83")
})

test_that("negative weight produces error", {
  expect_error(dlco_pred(50, 175, -70, 1, "DLCO"), "Weight must be positive")
})

test_that("invalid param produces error", {
  expect_error(dlco_pred(50, 175, 70, 1, "FEV1"), "Unknown DLCO parameter")
})

test_that("param names are case-insensitive", {
  expect_equal(
    dlco_pred(50, 175, 70, 1, "dlco"),
    dlco_pred(50, 175, 70, 1, "DLCO")
  )
})

test_that("mismatched input lengths produce error", {
  expect_error(dlco_pred(c(30, 50), c(170, 175, 180), 70, 1, "DLCO"),
               "same length")
})
