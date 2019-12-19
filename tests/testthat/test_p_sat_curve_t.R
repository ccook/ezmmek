library(testthat)
library(ezmmek)

context("p_sat_curve, clean")

test_that("dataframe input", {
  expect_output(p_sat_curve(d_std, d_sat))
})
