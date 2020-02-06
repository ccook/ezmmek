
##' @export

s_calc_act <- function(df_sat,
                       time.units = NULL,
                       spec.units = NULL,
                       df_std = NULL,
                       sub.type = NULL) {


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

    ### Create data frame with rate of reaction in terms of spec
    df_sat_activity <- df_sat %>%
      dplyr::group_by(sub.conc, replicate) %>%
      tidyr::nest() %>%
      dplyr::mutate(activity = purrr::map_dbl(data, function(df) coef(lm(spec ~ time, data = df))[2])) %>%
      dplyr::mutate(activity.m = mean(activity), activity.sd = sd(activity))

    ### Create vector of units
    units <- c("time.units" = time.units,
               "spec.units" = spec.units)

    ### Create vector of substrate type
    substrate <- c("sub.type" = sub.type)

    out_list <- list(s_act_raw_data = df_sat,
                     s_act_units = units,
                     s_act_activity_data = df_sat_slope,
                     s_act_substrate_type = substrate)

    class(out_list) <- c("ezmmek_s_act", "list")

    out_list

  } else {

    ### Stop function if d_std or d_sat columns lack these specific names
    assertable::assert_colnames(data = df_std,
                                colnames = c("std.conc", "spec"),
                                only_colnames = FALSE,
                                quiet = TRUE)

    ### Run function that creates standard curve object
    std_obj <- s_calc_std(df_std)

    ### Convert fsu to conc. of standard and add to d_sat dataframe
    lm.intercept <- coef(std_obj$s_std_curve_lm_fit)[1]
    lm.slope <- coef(std_obj$s_std_curve_lm_fit)[2]
    df_sat$spec.to.std <- (df_sat$spec - lm.intercept)/lm.slope

    df_sat_slope <- df_sat %>% dplyr::group_by(sub.conc, replicate) %>%
      tidyr::nest() %>%
      dplyr::mutate(activity = purrr::map_dbl(data, function(df) coef(lm(spec.to.std ~ time, data = df))[2])) %>%
      dplyr::group_by(sub.conc) %>%
      dplyr::mutate(activity.m = mean(activity), activity.sd = sd(activity))


    ### Create vector of units
    units <- c("time.units" = time.units, "spec.units" = spec.units)

    out_list <- list(s_act_raw_data = df_sat,
                     s_act_units = units,
                     s_act_activity_data = df_sat_slope,
                     s_act_substrate_type = substrate)

    class(out_list) <- c("ezmmek_s_activity", "list")

    out_list

  }

}
