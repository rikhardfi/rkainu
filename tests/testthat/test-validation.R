# Tests against published values from Kainu et al. (2015)
# Tolerance: 0.02 for volumes, 0.005 for ratios

test_that("predicted FEV1 matches published values", {
  # Male, 50 yr, 177 cm
  expect_equal(kainu_pred(50, 177, 1, "FEV1"), 4.046, tolerance = 0.02)
  # Female, 50 yr, 164 cm
  expect_equal(kainu_pred(50, 164, 2, "FEV1"), 2.925, tolerance = 0.02)
})

test_that("predicted FVC matches published values", {
  expect_equal(kainu_pred(50, 177, 1, "FVC"), 5.203, tolerance = 0.02)
  expect_equal(kainu_pred(50, 164, 2, "FVC"), 3.697, tolerance = 0.02)
})

test_that("predicted FEV1/FVC matches published values", {
  expect_equal(kainu_pred(50, 177, 1, "FEV1FVC"), 0.778, tolerance = 0.005)
  expect_equal(kainu_pred(50, 164, 2, "FEV1FVC"), 0.788, tolerance = 0.005)
})

test_that("LLN is predicted - 1.645 * SD", {
  pred <- kainu_pred(50, 177, 1, "FEV1")
  lln <- kainu_lln(50, 177, 1, "FEV1")
  expect_true(lln < pred)
  expect_equal(kainu_lln(50, 177, 1, "FEV1"), 3.168, tolerance = 0.02)
})

test_that("z-score is approximately 0 when observed equals predicted", {
  pred <- kainu_pred(50, 177, 1, "FEV1")
  z <- kainu_zscore(50, 177, 1, FEV1 = pred)
  expect_equal(z, 0, tolerance = 1e-10)
})

test_that("percent predicted is 100 when observed equals predicted", {
  pred <- kainu_pred(50, 177, 1, "FEV1")
  pp <- kainu_pctpred(50, 177, 1, FEV1 = pred)
  expect_equal(pp, 100, tolerance = 1e-10)
})

test_that("FEV1 declines with age", {
  young <- kainu_pred(30, 177, 1, "FEV1")
  mid <- kainu_pred(50, 177, 1, "FEV1")
  old <- kainu_pred(70, 177, 1, "FEV1")
  expect_true(young > mid)
  expect_true(mid > old)
})

test_that("FVC increases with height", {
  short <- kainu_pred(50, 160, 1, "FVC")
  tall <- kainu_pred(50, 190, 1, "FVC")
  expect_true(tall > short)
})

test_that("all 10 parameters produce reasonable values for males", {
  res <- kainu_pred(50, 177, 1,
    c("FVC", "FEV1", "FEV1FVC", "FEV6", "FEV1FEV6",
      "PEF", "MMEF", "MEF75", "MEF50", "MEF25"))
  expect_equal(ncol(res), 10)
  expect_true(all(res > 0))
  # Volumes should be > 1 L

  expect_true(res$FVC > 1)
  expect_true(res$FEV1 > 1)
  # Ratios should be < 1
  expect_true(res$FEV1FVC < 1)
  expect_true(res$FEV1FEV6 < 1)
})

test_that("all 10 parameters produce reasonable values for females", {
  res <- kainu_pred(50, 164, 2,
    c("FVC", "FEV1", "FEV1FVC", "FEV6", "FEV1FEV6",
      "PEF", "MMEF", "MEF75", "MEF50", "MEF25"))
  expect_equal(ncol(res), 10)
  expect_true(all(res > 0))
  expect_true(res$FVC > 1)
  expect_true(res$FEV1 > 1)
  expect_true(res$FEV1FVC < 1)
})
