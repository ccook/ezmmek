library(testthat)
library(ezmmek)

context("s_sat_curve, error")

test_that("dataframe input", {
  expect_error(s_sat_curve(d_std, d_std))
})
