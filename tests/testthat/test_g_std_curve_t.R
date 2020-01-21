library(testthat)
library(ezmmek)

context("g_std_curve, clean")

test_that("dataframe input", {
  expect_output(g_std_curve(d_std_g))
})
