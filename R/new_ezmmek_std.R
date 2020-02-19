##' @export

new_ezmmek_std <- function(df_std,
                           std.conc.units = NA,
                           std.spec.units = NA,
                           method = NA,
                           other_arg = NA) {

  if(is.na(method)) {
    stop("Must assign method as 'german' or 'steen'")
  }

  ### Will only accept a data frame
  assertthat::are_equal(class(df_std), "data.frame")

  ### Create a vector of units
  units <- c("std.conc.units" = std.conc.units,
             "std.spec.units" = std.spec.units)

  ### Use self-made function to calculate standard fits
  calc_std_fit <- calc_std_fit(df_std, method)

if(method == "steen") {

  ### Output list of containing linear model, units, and dataframe
  out_list <- list(
    std_lm_fit = calc_std_fit$lm_fit,
    std_data = df_std,
    std_units = units,
    method = method
  )
}

  if(method == "german") {
    out_list <- list(
      std_lm_fit_homo = calc_std_fit$lm_fit_homo,
      std_lm_fit_buffer = calc_std_fit$lm_fit_buffer,
      std_data = df_std,
      std_units = units,
      method = method
    )
  }


out_list <- list(std_obj = out_list,
                     other_arg = other_arg)


  class(out_list) <- c("ezmmek_std", "list")

   out_list

}



