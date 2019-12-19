library(testthat)
library(ezmmek)

context("p_sat_raw, error")

test_that("dataframe input", {
  expect_error(p_sat_raw(d_std))
})
