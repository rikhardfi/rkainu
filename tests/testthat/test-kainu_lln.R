test_that("kainu_lln returns numeric for single param", {
  res <- kainu_lln(50, 177, 1, "FEV1")
  expect_type(res, "double")
  expect_length(res, 1)
})

test_that("LLN is always less than predicted", {
  params <- c("FVC", "FEV1", "FEV1FVC", "PEF", "MMEF")
  for (p in params) {
    pred <- kainu_pred(50, 177, 1, p)
    lln <- kainu_lln(50, 177, 1, p)
    expect_true(lln < pred, info = paste("LLN < pred for", p))
  }
})

test_that("kainu_lln returns data.frame for multiple params", {
  res <- kainu_lln(50, 177, 1, c("FEV1", "FVC"))
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("FEV1", "FVC"))
})
