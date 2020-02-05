##' @export

s_calc_raw <- function(df_sat, time.units = NULL, spec.units = NULL, df_std = NULL) {


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
    df_sat_slope <- df_sat %>%
      group_by(sub.conc, replicate) %>%
      nest() %>%
      dplyr::mutate(slope = purrr::map_dbl(data, function(df) coef(lm(spec ~ time, data = df))[2]))

    ### Create vector of units
    units <- c("time.units" = time.units, "spec.units" = spec.units)

    out_list <- list(s_raw_data = df, s_raw_units = units, s_raw_slopes = df_sat_slope)

    class(out_list) <- c("ezmmek_s_raw_data", "list")

    out_list

  } else {

    std_obj <- s_calc_std(df_std)

    ### Convert fsu to conc. of standard and add to d_sat dataframe
    lm.intercept <- coef(std_obj$s_std_curve_lm_fit)[1]
    lm.slope <- coef(std_obj$s_std_curve_lm_fit)[2]
    df_sat$spec.to.std <- (df_sat$spec - lm.intercept)/lm.slope

    df_sat_slope <- df_sat %>% dplyr::group_by(sub.conc, replicate) %>%
      tidyr::nest() %>%
      dplyr::mutate(std.slope = purrr::map_dbl(data, function(df) coef(lm(spec.to.std ~ time, data = df))[2])) %>%
      dplyr::group_by(sub.conc) %>%
      dplyr::mutate(slope.m = mean(std.slope), slope.sd = sd(std.slope))
  }

  ### Create vector of units
  units <- c("time.units" = time.units, "spec.units" = spec.units)

  out_list <- list(s_raw_data = df, s_raw_units = units, s_raw_slopes = df_sat_slope)

  class(out_list) <- c("ezmmek_s_raw_data", "list")

  out_list

}
