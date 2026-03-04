test_that("kainu_pred returns numeric for single param", {
  res <- kainu_pred(50, 177, 1, "FEV1")
  expect_type(res, "double")
  expect_length(res, 1)
})

test_that("kainu_pred returns data.frame for multiple params", {
  res <- kainu_pred(50, 177, 1, c("FEV1", "FVC"))
  expect_s3_class(res, "data.frame")
  expect_equal(ncol(res), 2)
  expect_equal(names(res), c("FEV1", "FVC"))
})

test_that("kainu_pred is vectorized over age", {
  res <- kainu_pred(c(30, 50, 70), 177, 1, "FEV1")
  expect_length(res, 3)
  expect_true(all(diff(res) < 0))  # Declining with age
})

test_that("kainu_pred is vectorized over height", {
  res <- kainu_pred(50, c(160, 170, 180), 1, "FVC")
  expect_length(res, 3)
  expect_true(all(diff(res) > 0))  # Increasing with height
})

test_that("kainu_pred recycles length-1 inputs", {
  res <- kainu_pred(c(30, 50), 177, 1, "FEV1")
  expect_length(res, 2)
})

test_that("kainu_pred handles mixed sex", {
  res <- kainu_pred(50, 170, c(1, 2), "FEV1")
  expect_length(res, 2)
  expect_true(res[1] > res[2])  # Male > female at same height
})

test_that("kainu_pred parameter names are case-insensitive", {
  expect_equal(
    kainu_pred(50, 177, 1, "fev1"),
    kainu_pred(50, 177, 1, "FEV1")
  )
})
