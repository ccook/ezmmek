library(testthat)
library(ezmmek)

context("s_std_curve, clean")

test_that("dataframe input", {
  expect_output(s_std_curve(d_std, x.label = "test", y.label = "test"))
})
