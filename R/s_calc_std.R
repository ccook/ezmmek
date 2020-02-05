##' @export

s_calc_std <- function(df, conc.units = NULL, spec.units = NULL) {

  ### Will only accept a data frame
  assertthat::are_equal(class(df), "data.frame")

  ### Stop function if columns in data frame lack these specific names
  assertable::assert_colnames(data = df,
                              colnames = c("std.conc", "spec"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Fit linear model
  s_std_curve_lm_fit <- lm(formula = spec ~ std.conc, data = df)

  ### Create vector of units
  units <- c("conc.units" = conc.units, "spec.units" = spec.units)

  ### Output list of containing linear model and data frame
  out_list <- list(s_std_curve_lm_fit = s_std_curve_lm_fit,
                   s_std_curve_data = df,
                   s_std_curve_units = units)

  ### Assign list to class of 'list' and subclass of 'ezmmek_s_std_curve
  class(out_list) <- c("ezmmek_s_std_curve", "list")

  ### Output list
  out_list

}

