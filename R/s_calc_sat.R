##' @export

s_calc_sat <- function(df1, df2) {

  ### Will only accept a data frame for standard curve data
  assertthat::are_equal(class(df1), "data.frame")

  ### Will only accept a data frame for saturation curve data
  assertthat::are_equal(class(df2), "data.frame")

  ### Stop function if columns in stanard curve data frame lack these specific names
  assertable::assert_colnames(data = df1,
                              colnames = c("std.conc", "spec"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Stop function if columns in saturation curve data frame lack these specific names
  assertable::assert_colnames(data = d_sat,
                              colnames = c("time",
                                           "replicate",
                                           "spec",
                                           "sub.conc"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Fit linear model to standard curve
  s_std_curve_lm_fit <- lm(formula = spec ~ std.conc, data = df)









  }
