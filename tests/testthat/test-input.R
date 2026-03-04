test_that("height in metres is auto-detected and converted", {
  expect_message(
    res <- kainu_pred(50, 1.77, 1, "FEV1"),
    "metres"
  )
  res_cm <- suppressMessages(kainu_pred(50, 177, 1, "FEV1"))
  expect_equal(res, res_cm)
})

test_that("invalid sex produces error", {
  expect_error(kainu_pred(50, 177, 3, "FEV1"), "1 \\(male\\) or 2 \\(female\\)")
  expect_error(kainu_pred(50, 177, 0, "FEV1"), "1 \\(male\\) or 2 \\(female\\)")
})

test_that("age outside 18-90 produces warning", {
  expect_warning(kainu_pred(15, 177, 1, "FEV1"), "18-90")
  expect_warning(kainu_pred(95, 177, 1, "FEV1"), "18-90")
})

test_that("age outside range still computes", {
  res <- suppressWarnings(kainu_pred(95, 177, 1, "FEV1"))
  expect_type(res, "double")
  expect_true(is.finite(res))
  expect_true(res > 0)
})

test_that("invalid param produces error", {
  expect_error(kainu_pred(50, 177, 1, "DLCO"), "Unknown parameter")
})

test_that("mismatched input lengths produce error", {
  expect_error(kainu_pred(c(30, 50), c(170, 175, 180), 1, "FEV1"),
               "same length")
})

test_that("kainu_df adds correct columns", {
  df <- data.frame(
    age = c(30, 50),
    height = c(180, 175),
    sex = c(1, 2),
    FEV1 = c(4.5, 2.9)
  )
  res <- kainu_df(df, params = "FEV1")
  expect_true("pred_FEV1" %in% names(res))
  expect_true("lln_FEV1" %in% names(res))
  expect_true("z_FEV1" %in% names(res))
  expect_true("pctpred_FEV1" %in% names(res))
  expect_equal(nrow(res), 2)
})

test_that("kainu_df skips zscore/pctpred when observed column missing", {
  df <- data.frame(age = 50, height = 177, sex = 1)
  res <- kainu_df(df, params = "FEV1")
  expect_true("pred_FEV1" %in% names(res))
  expect_true("lln_FEV1" %in% names(res))
  expect_false("z_FEV1" %in% names(res))
  expect_false("pctpred_FEV1" %in% names(res))
})

test_that("spline clamping works for extreme ages", {
  # Age 18 and 84 are table boundaries
  expect_true(is.finite(suppressWarnings(kainu_pred(17, 177, 1, "FEV1"))))
  expect_true(is.finite(suppressWarnings(kainu_pred(85, 177, 1, "FEV1"))))
})
