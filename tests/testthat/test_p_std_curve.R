library(testthat)
library(ezmmek)

context("p_std_curve")

test_that("dataframe input", {
  expect_error(p_std_curve(d_sat))
})
