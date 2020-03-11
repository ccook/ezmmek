#' ezmmek_calibrate_activities
#'
#' @importFrom magrittr "%>%"
#'
#' @description Calibrates enzyme activity data by standard curve
#'
#' @param df Joined dataframes of class 'new_ezmmek_std_group' and 'new_ezmmek_act_group'
#' @param method Enzyme assay protocol. Must define method as 'steen' or 'german'
#' @param columns Column names carried from parent functions

########
### Calibrate activities by standard curve data
########

ezmmek_calibrate_activities <- function(df, method, columns) {

  if(method == "steen") {
    ### Calibrates raw activity data by standard curve
    std_act_calibrated <- df %>%
      tidyr::unnest(act.raw.data.s) %>%
      dplyr::mutate(signal.calibrated = (signal - std.lm.homo.intercept) / std.lm.homo.slope) %>% #calibrate signal
      tidyr::nest(act.calibrated.data = c(time, signal, signal.calibrated)) %>% #place calibrated signal back in nested df
      dplyr::mutate(activity = purrr::map_dbl(act.calibrated.data,  #calculate slope of calibrated data
                                       function(df) coef(lm(signal.calibrated ~ time,
                                                            data = df))[2])) %>%
      dplyr::group_by_at(dplyr::vars(sub.conc, sub.type, intersect(names(.), columns))) %>%
      dplyr::mutate(activity.m = mean(activity), #calculate means and sd's of activities
             activity.sd = sd(activity))
  }

  if(method == "german") {
    std_act_calibrated <- df %>%
      tidyr::unnest(act.raw.data.g) %>%
      dplyr::mutate(emission.coef = std.lm.homo.slope / assay.vol,
                    net.signal = (signal - homo.control) / quench.coef - sub.control,
                    activity = (net.signal * buffer.vol) / (emission.coef * homo.vol * time * soil.mass)) %>%
      dplyr::group_by(sub.conc) %>%
      dplyr::mutate(activity.m = mean(activity), activity.sd = sd(activity))
  }
  std_act_calibrated
}
