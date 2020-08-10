library(testthat)
library(ezmmek)

test_that("make ezmmek object", {
  expect_s3_class(test <- new_ezmmek_act_calibrate("test_data/tyson_std_04172020.csv",
                                                   "test_data/tyson_sat_german_04172020.csv",
                                                   std_type,
                                                   site_name,
                                                   substrate_type,
                                                   method = "ibc"),
                  c("new_ezmmek_calibrate", "data.frame"),
                  exact = TRUE)
})
