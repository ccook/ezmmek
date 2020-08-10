library(testthat)
library(ezmmek)

test_that("make ezmmek object", {
  expect_s3_class(test <- new_ezmmek_std_group("test_data/tyson_std_04172020.csv",
                                               std_type,
                                               site_name,
                                               substrate_type,
                                               method = "isc"),
                  c("new_ezmmek_std_group", "data.frame"),
                  exact = TRUE)
})
