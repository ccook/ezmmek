library(testthat)
library(ezmmek)

context("s_sat_curve, clean")

test_that("dataframe input", {
  expect_output(s_sat_curve(d_std, d_sat, x.label = "test", y.label = "test"))
})
