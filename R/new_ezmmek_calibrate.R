########
### Join activity dataframe with standard dataframe and calibrate
########

new_ezmmek_act_calibrate <- function(std.data.fn,
                                     act.data.fn,
                                     ...,
                                     method = NA,
                                     conc.units = NA,
                                     signal.units = NA,
                                     columns = NULL) {

  ### Use '...' arguments if column names not supplied in parent fxn
  if(is.null(columns)) {
    columns <- map_chr(enquos(...), rlang::quo_name)
  }
  ### Creates dataframe of standard curve data
  std_data_grouped <- new_ezmmek_std_group(std.data.fn,
                                           method = method,
                                           conc.units,
                                           signal.units,
                                           columns = columns)

  ### Creates dataframe of raw activity data
  act_data_grouped <- new_ezmmek_act_group(act.data.fn,
                                           method = method,
                                           columns = columns)

  ### Joins the two data frames based on common descriptor columns
  std_act_std <- full_join(act_data_grouped, std_data_grouped)

  ### Calibrate activities
  std_act_calibrated <- calibrate_activities(std_act_std, method,columns)

   ### Assign new class
  class(std_act_calibrated) <- c("new_ezmmek_calibrate", "data.frame")

  std_act_calibrated

}
