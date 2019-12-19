library(testthat)
library(ezmmek)

context("p_std_curve, clean")

test_that("dataframe input", {
  expect_output(p_std_curve(d_std))
})
