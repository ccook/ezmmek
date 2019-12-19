library(testthat)
library(ezmmek)

context("p_sat_curve, error")

test_that("dataframe input", {
  expect_error(p_sat_curve(d_std, d_std))
})
