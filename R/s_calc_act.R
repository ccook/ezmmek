##' @export

s_calc_act <- function(df_sat,
                       act.time.units = NULL,
                       act.spec.units = NULL,
                       std.conc.units = NULL,
                       std.spec.units = NULL,
                       df_std = NULL,
                       ...) {

  ### Will only accept a data frame
  assertthat::are_equal(class(df_sat), "data.frame")

  ### Stop function if columns lack these specific names
  assertable::assert_colnames(data = df_sat,
                              colnames = c("time",
                                           "replicate",
                                           "spec",
                                           "sub.conc"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  if(is.null(df_std)) {

    ### Create vector of units
    units <- c("act.time.units" = act.time.units,
               "act.spec.units" = act.spec.units)


    out_list <- list(s_act_raw_data = df_sat,
                     s_act_units = units,
                     ...)


    class(out_list) <- c("ezmmek_s_act", "list")

    out_list

  } else {

    ### Stop function if d_std or d_sat columns lack these specific names
    assertable::assert_colnames(data = df_std,
                                colnames = c("std.conc", "spec"),
                                only_colnames = FALSE,
                                quiet = TRUE)

    ### Run function that creates standard curve object
    std_obj <- s_calc_std(df_std,
                          std.conc.units,
                          std.spec.units,
                          ...)

    ### Create vector of units
    units <- c("act.time.units" = act.time.units,
               "act.spec.units" = act.spec.units)


    out_list <- list(s_act_raw_data = df_sat,
                     s_act_units = units,
                     s_act_std_object = std_obj)

    class(out_list) <- c("ezmmek_s_act", "list")

    out_list

  }

}
