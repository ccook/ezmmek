##' @export

s_calc_std <- function(df_std,
                       std.conc.units = NULL,
                       std.spec.units = NULL,
                       ...) {

  ### Will only accept a data frame
  assertthat::are_equal(class(df_std), "data.frame")

  ### Stop function if columns in data frame lack these specific names
  assertable::assert_colnames(data = df_std,
                              colnames = c("std.conc", "spec"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Fit linear model
  lm_fit <- lm(formula = spec ~ std.conc, data = df_std)

  ### Create vector of units
  units <- c("std.conc.units" = std.conc.units, "std.spec.units" = std.spec.units)

  ### Output list of containing linear model and data frame
  out_list <- list(s_std_lm_fit = lm_fit,
                   s_std_data = df_std,
                   s_std_units = units,
                   ...)

  ### Assign list to class of 'list' and subclass of 'ezmmek_s_std_curve'
  class(out_list) <- c("ezmmek_s_std", "list")

  out_list

}

