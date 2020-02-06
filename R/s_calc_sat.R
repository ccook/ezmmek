##' @export

s_calc_sat <- function(df_std,
                       df_sat,
                       rate.units = NULL,
                       sub.conc.units = NULL,
                       sub.type = NULL) {

  ### Will only accept a data frame for standard curve data
  assertthat::are_equal(class(df_std), "data.frame")

  ### Will only accept a data frame for activity data
  assertthat::are_equal(class(df_sat), "data.frame")

  ### Stop function if d_std or d_sat columns lack these specific names
  assertable::assert_colnames(data = df_std,
                              colnames = c("std.conc", "spec"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Stop function if columns lack these specific names
  assertable::assert_colnames(data = df_sat,
                              colnames = c("time",
                                           "replicate",
                                           "spec",
                                           "sub.conc"),
                              only_colnames = FALSE,
                              quiet = TRUE)


  ### Run function that creates standard curve object
  std_obj <- s_calc_std(df_std)

  ### Run function that creates activity object
  act_obj <- s_calc_act(df_sat)

  ### Convert spec to conc. of standard and add to df_sat dataframe
  lm.intercept <- coef(std_obj$s_std_lm_fit)[1]
  lm.slope <- coef(std_obj$s_std_lm_fit)[2]
  df_sat$spec.to.std <- (df_sat$spec - lm.intercept)/lm.slope

  df_sat_activity <- df_sat %>% dplyr::group_by(sub.conc, replicate) %>%
    tidyr::nest() %>%
    dplyr::mutate(activity = purrr::map_dbl(data, function(df) coef(lm(spec.to.std ~ time, data = df))[2])) %>%
    dplyr::group_by(sub.conc) %>%
    dplyr::mutate(activity.m = mean(activity), activity.sd = sd(activity))

  ### Assign starting values to predict km and vmax
  max.activity.m <- max(df_sat$activity.m)
  half.conc <- median(df_sat$sub.conc)

  ### Predict km and vmax values
  mm_form <- formula(activity.m ~ (vmax * sub.conc)/(km + sub.conc))
  mm_fit <- nls2::nls2(formula = mm_form, data = df_sat_activity,
                       start = list(vmax = max.activity.m, km = half.conc))



  ### Create vector of units
  units <- c("rate.units" = rate.units,
             "sub.conc.units" = sub.conc.units)

  ### Create vector of substrate type
  substrate <- c("sub.type" = sub.type)

  out_list <- list(s_sat_activity_data = df_sat_activity,
                   s_sat_units = units,
                   s_sat_substrate_type = substrate,
                   s_sat_std_object = std_obj,
                   s_sat_act_object = act_obj)

}
