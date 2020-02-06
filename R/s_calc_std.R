##' @export

s_calc_std <- function(df_std,
                       conc.units = NULL,
                       spec.units = NULL,
                       std.type = NULL,
                       site.name = NULL) {

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
  units <- c("conc.units" = conc.units, "spec.units" = spec.units)

  ### Create vector of standard type
  standard <- c("std.type" = std.type)

  ### Create vector of site name
  site <- c("site.name" = site.name)

  ### Output list of containing linear model and data frame
  out_list <- list(s_std_lm_fit = lm_fit,
                   s_std_raw_data = df_std,
                   s_std_units = units,
                   s_std_standard_type = standard,
                   s_std_site_name = site)

  ### Assign list to class of 'list' and subclass of 'ezmmek_s_std_curve'
  class(out_list) <- c("ezmmek_s_std_curve", "list")

  ### Output list
  out_list

}

