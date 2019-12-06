library(testthat)
library(ezmmek)

context("p_sat_raw")

test_that("dataframe input", {
  expect_error(p_sat_raw(d_std))
})
