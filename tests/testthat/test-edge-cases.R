test_that("negative age produces error", {
  expect_error(kainu_pred(-5, 177, 1, "FEV1"), "Age must be positive")
})

test_that("negative height produces error", {
  expect_error(kainu_pred(50, -170, 1, "FEV1"), "Height must be positive")
})

test_that("NA in observed values for kainu_zscore returns NA", {
  result <- kainu_zscore(50, 177, 1, FEV1 = NA_real_)
  expect_true(is.na(result))
})

test_that("boundary ages (18, 84) produce finite results", {
  res_18 <- kainu_pred(18, 177, 1, "FEV1")
  res_84 <- kainu_pred(84, 177, 1, "FEV1")
  expect_true(is.finite(res_18))
  expect_true(is.finite(res_84))
  expect_true(res_18 > 0)
  expect_true(res_84 > 0)
})

test_that("age 100 produces finite results (spline clamping)", {
  res <- suppressWarnings(kainu_pred(100, 177, 1, "FEV1"))
  expect_true(is.finite(res))
  expect_true(res > 0)
})

test_that("mixed NA/valid in vectorized kainu_pred calls", {
  ages <- c(30, NA, 50)
  heights <- c(170, 175, NA)
  # NA inputs propagate through log() and produce NaN/NA in output

  res <- suppressWarnings(kainu_pred(ages, heights, 1, "FEV1"))
  expect_length(res, 3)
  expect_true(is.finite(res[1]))
  # Entries with NA age or height should not be finite
  expect_false(is.finite(res[2]))
  expect_false(is.finite(res[3]))
})
