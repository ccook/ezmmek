library(testthat)
library(ezmmek)

test_that("csv files do not align with method", {
  expect_error(test <- new_ezmmek_sat_fit("test_data/tyson_std_04172020.csv",
                                          "test_data/tyson_sat_german_04172020.csv",
                                          std_type,
                                          site_name,
                                          substrate_type,
                                          method = "isc")
  )
})
