library(testthat)
library(ezmmek)

context("s_std_curve, error")

test_that("dataframe input", {
  expect_error(s_std_curve(d_sat))
})
