library(testthat)
library(ezmmek)

context("sat_raw, error")

test_that("dataframe input", {
  expect_error(sat_raw(d_std))
})
