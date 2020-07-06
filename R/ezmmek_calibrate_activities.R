#' ezmmek_calibrate_activities
#'
#' @importFrom magrittr "%>%"
#'
#' @description Calibrates enzyme activity data by standard curve
#'
#' @param df Joined dataframes of class 'new_ezmmek_std_group' and 'new_ezmmek_act_group'
#' @param method Enzyme assay protocol. Must define method as 'isc' or 'ibc'
#' @param columns Column names carried from parent functions

########
### Calibrate activities by standard curve data
########

ezmmek_calibrate_activities <- function(df, method, columns) {

  if(method == "isc") {
    ### Calibrates raw activity data by standard curve
    std_act_calibrated <- df %>%
      tidyr::unnest(act_raw_data_isc) %>%
      dplyr::mutate(signal_calibrated = ((signal - std_lm_homo_buffer_intercept) / std_lm_homo_buffer_slope) -
                      (kill_control - std_lm_homo_buffer_intercept) / std_lm_homo_buffer_slope) %>% #calibrate signal
      tidyr::nest(act_calibrated_data = c(time, signal, kill_control, signal_calibrated)) %>% #place calibrated signal back in nested df
      dplyr::mutate(activity = purrr::map_dbl(act_calibrated_data,  #calculate slope of calibrated data
                                              function(df) coef(lm(signal_calibrated ~ time,
                                                                   data = df))[2])) %>%
      dplyr::group_by_at(dplyr::vars(substrate_conc, substrate_type, intersect(names(.), columns))) %>%
      dplyr::mutate(activity_m = mean(activity), #calculate means and sd's of activities
                    activity_sd = sd(activity)) %>%
      tidyr::unnest(act_calibrated_data) %>%
      tidyr::nest(act_calibrated_data_isc = c(substrate_conc,
                           replicate,
                           time,
                           signal,
                           kill_control,
                           signal_calibrated,
                           activity,
                           activity_m,
                           activity_sd))
  }

  if(method == "ibc") {
    std_act_calibrated <- df %>%
      tidyr::unnest(act_raw_data_ibc) %>%
      dplyr::mutate(emission_coef = std_lm_homo_slope / std_vol, #emission coefficient
                    net_signal = (signal - homo_control) / quench_coef - substrate_control, #net signal
                    activity = (net_signal * buffer_vol) / (emission_coef * homo_vol * time * soil_mass)) %>% #activity
      dplyr::group_by(substrate_conc) %>%
      dplyr::mutate(activity_m = mean(activity), activity_sd = sd(activity)) %>% #mean and sd of activity
      tidyr::nest(act_calibrated_data_ibc = c(substrate_conc,
                                          replicate,
                                          time,
                                          signal,
                                          buffer_vol,
                                          homo_vol,
                                          soil_mass,
                                          std_vol,
                                          homo_control,
                                          substrate_control,
                                          net_signal,
                                          activity,
                                          activity_m,
                                          activity_sd))

  }
  std_act_calibrated
}
