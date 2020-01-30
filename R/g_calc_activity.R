##' Calculates activity via the German et al method
##'
##' @export
##' @references Insert German reference
##'

g_calc_activity <- function(d_std_g, d_act_g, x.label = NULL, y.label = NULL) {


  ### Stop function if columns lack these specific names
  assertable::assert_colnames(data = d_std_g,
                              colnames = c("std.conc", "spec.homo", "spec.buffer", "replicate"),
                              only_colnames = FALSE,
                              quiet = TRUE)


  assertable::assert_colnames(data = d_act_g,
                              colnames = c("time",
                                           "replicate",
                                           "spec",
                                           "sub.conc",
                                           "assay.vol",
                                           "sub.control",
                                           "buffer.vol",
                                           "homo.vol",
                                           "soil.mass"), # probably change to solid.mass or something so it makes sense with sediments (or not; ok just to refer to it as soil under the hood as long as we don't let the user see)
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Add mean spec values of standard in buffer and standard in homogenate
  #d_std_g_2 <- d_std_g #%>%
    #dplyr::group_by(std.conc) #%>%
    #dplyr::mutate(spec.m.homo = mean(spec.homo)) %>% #
    #dplyr::mutate(spec.m.buffer = mean(spec.buffer))

  # Wrap this in a function so that it deals properly with errors
  ### Calculate slope of mean spec value of standard in homogenate
  lm_fit_homo <- calc_std_curve(d_std_curve, ...)
  #lm_fit_homo <- lm(spec.homo ~ std.conc, data = d_std_g_2) # Need to wrap this in a function so that if there is an error, your system won't fail
  #lm.slope.homo <- coef(lm_fit_homo)[2]

  ### Calculate slope of mean spec value of standard in buffer
  lm_fit_buffer <- lm(spec.buffer ~ std.conc, data = d_std_g_2)
  lm.slope.buffer <- coef(lm_fit_buffer)[2]

  ### Add quench coef. and emission coef. to raw saturation data
  d_sat_g_2 <- d_sat_g %>%
    dplyr::group_by(sub.conc, time) %>%
    dplyr::mutate(quench.coef = lm.slope.homo / lm.slope.buffer) %>%
    dplyr::mutate(emission.coef = (lm.slope.homo / assay.vol))

  ### Create new dataframe that contains average spec values of homogenate controls over time
  d_sat_baseline <- d_sat_g_2 %>%
    filter(sub.conc == 0) %>%
    group_by(time) %>%
    summarize(spec.baseline.m = mean(spec),
              spec.baseline.sd = sd(spec))

  ### Create new dataframe that merges d_sat_g_2 with homogenate control values from d_sat_baseline
  d_sat_g_3 <- merge(d_sat_g_2, d_sat_baseline, by = ("time")) %>%
    dplyr::filter(time > 0) #Filter out Time 0, because cannot divide by 0 later

  ### Correct spec values by homogenate baseline control
  d_sat_g_3$spec.homogenate.corrected <- d_sat_g_3$spec - d_sat_g_3$spec.baseline.m

  ### Create new dataframe that calculate net fluorescence and activity
  d_sat_g_4 <- d_sat_g_3 %>%
    dplyr::mutate(net.fluor = (spec.homogenate.corrected / quench.coef) - sub.control) %>%
    dplyr::mutate(activity = (net.fluor * buffer.vol) / (emission.coef * homo.vol * time * soil.mass))

  ### Create new dataframe that contains mean and standard deviations of activity by substrate concentration
  d_sat_g_5 <- d_sat_g_4 %>%
    dplyr::group_by(sub.conc) %>%
    dplyr::mutate(activity.m = mean(activity), activity.sd = sd(activity)) %>%
    dplyr::filter(sub.conc > 0)

}
