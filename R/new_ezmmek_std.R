##' @export

new_ezmmek_std <- function(std.data.fn,
                           std.conc.units = NA,
                           std.spec.units = NA,
                           method = NA,
                           std.other.arg = NA,
                           ...) {

  #blah <- list(...)

  read_std_data <- read_std_data(std.data.fn, ...)

  std_data_grouped <- read_std_data$std_data_grouped

  if(is.na(method)) {
    stop("Must assign method as 'german' or 'steen'")
  }

  ### Create a vector of units
  units <- c("std.conc.units" = std.conc.units,
             "std.spec.units" = std.spec.units)

  ### Use self-made function to calculate standard fits
  calc_std_fit <- calc_std_fit(std_data_grouped, method)

  if(method == "steen") {

    ### Output list of containing linear model, units, and dataframe
    out_list <- list(
      std_lm_fit = calc_std_fit$lm_fit,
      std_data = std_data_grouped,
      std_units = units,
      method = method
    )
  }

  if(method == "german") {
    out_list <- list(
      std_lm_fit_homo = calc_std_fit$lm_fit_homo,
      std_lm_fit_buffer = calc_std_fit$lm_fit_buffer,
      std_data = std_data_grouped,
      std_units = units,
      method = method
    )
  }


  out_list <- list(std_obj = out_list,
                   std.other.arg = std.other.arg)


  class(out_list) <- c("ezmmek_std", "list")

  out_list

}



