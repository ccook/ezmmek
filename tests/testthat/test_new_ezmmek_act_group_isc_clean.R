library(testthat)
library(ezmmek)

test_that("make ezmmek object", {
  expect_s3_class(test <- new_ezmmek_act_group("test_data/tyson_sat_steen_04172020.csv",
                                               std_type,
                                               site_name,
                                               substrate_type,
                                               method = "isc"),
                  c("new_ezmmek_act_group", "data.frame"),
                  exact = TRUE)
})
