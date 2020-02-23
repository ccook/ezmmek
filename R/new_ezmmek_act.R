##' @export

new_ezmmek_act <- function(df_sat,
                           act.time.units = NA,
                           act.spec.units = NA,
                           std_obj = NULL,
                           std.conc.units = NA,
                           std.spec.units = NA,
                           method = NA,
                           other_arg = NA) {

  ### Will only accept a data frame
  assertthat::are_equal(class(df_sat), "data.frame")

  if(is.na(method)) {
    stop("Must assign method as 'german' or 'steen'")
  }

  units <- c("act.time.units" = act.time.units,
             "act.spec.units" = act.spec.units)

  if(is.null(std_obj)) {

    std_obj <- new_ezmmek_std(d_sat,
                              method,
                              std.conc.units,
                              std.spec.units)
  }

  calibrated_activities <- calibrate_activities(df_sat,
                                                std_obj,
                                                method)


  out_list <- list(act_raw_data = df_sat,
                   act_calibrated_data = as.data.frame(calibrated_activities[1]),
                   act_units = units,
                   std_list = std_obj,
                   method = method)

  out_list <- list(act_list = out_list,
                   other_arg = other_arg)

  class(out_list) <- c("ezmmek_act", "list")

  out_list

}
