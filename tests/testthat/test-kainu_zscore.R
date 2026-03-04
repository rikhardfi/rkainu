test_that("kainu_zscore returns numeric for single param", {
  res <- kainu_zscore(50, 177, 1, FEV1 = 4.0)
  expect_type(res, "double")
  expect_length(res, 1)
})

test_that("kainu_zscore returns data.frame for multiple params", {
  res <- kainu_zscore(50, 177, 1, FEV1 = 4.0, FVC = 5.0)
  expect_s3_class(res, "data.frame")
  expect_equal(ncol(res), 2)
})

test_that("positive z-score for above-predicted values", {
  pred <- kainu_pred(50, 177, 1, "FEV1")
  z <- kainu_zscore(50, 177, 1, FEV1 = pred + 0.5)
  expect_true(z > 0)
})

test_that("negative z-score for below-predicted values", {
  pred <- kainu_pred(50, 177, 1, "FEV1")
  z <- kainu_zscore(50, 177, 1, FEV1 = pred - 0.5)
  expect_true(z < 0)
})

test_that("kainu_zscore errors with no observed values", {
  expect_error(kainu_zscore(50, 177, 1), "At least one")
})
