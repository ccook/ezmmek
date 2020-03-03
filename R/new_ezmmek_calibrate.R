########
### Join activity dataframe with standard dataframe and calibrate
########

new_ezmmek_act_calibrate <- function(std.data.fn,
                                     act.data.fn,
                                     ...,
                                     method = NA,
                                     conc.units = NA,
                                     signal.units = NA) {

  columns <- map_chr(enquos(...), quo_name)

  std_data_grouped <- new_ezmmek_std_group(std.data.fn,
                                           method,
                                           conc.units,
                                           signal.units,
                                           columns = columns)

  act_data_grouped <- new_ezmmek_act_group(act.data.fn,
                                           method,
                                           columns = columns)


  std_act_std <- full_join(act_data_grouped, std_data_grouped)
  #std_act_std <- list(std_data_grouped = std_data_grouped,
                      #act_data_grouped = act_data_grouped)
  #std_act_std
}
