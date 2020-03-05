########
### Calibrate activities by standard curve data
########

calibrate_activities <- function(df, method, columns) {

  if(method == "steen") {
    ### Calibrates raw activity data by standard curve
    std_act_calibrated <- df %>%
      unnest(act.raw.data.s) %>%
      mutate(signal.calibrated = (signal - std.lm.homo.intercept) / std.lm.homo.slope) %>% #calibrate signal
      nest(act.calibrated.data = c(time, signal, signal.calibrated)) %>% #place calibrated signal back in nested df
      mutate(activity = map_dbl(act.calibrated.data,  #calculate slope of calibrated data
                                      function(df) coef(lm(signal.calibrated ~ time,
                                                           data = df))[2])) %>%
      group_by_at(vars(sub.conc, sub.type, intersect(names(.), columns))) %>%
      mutate(activity.m = mean(activity), #calculate means and sd's of activities
             activity.sd = sd(activity))
  }

  if(method == "german") {
    std_act_calibrated <- df %>%
      unnest(act.raw.data.g) %>%
      mutate(emission.coef = std.lm.homo.slope / assay.vol,
             net.signal = (signal - homo.control) / quench.coef - sub.control,
             activity = (net.signal * buffer.vol) / (emission.coef * homo.vol * time * soil.mass)) %>%
               group_by(sub.conc) %>%
             mutate(activity.m = mean(activity), activity.sd = sd(activity))
  }
  std_act_calibrated
}
