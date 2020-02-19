calibrate_activities <- function(df_sat,
                                 std_obj,
                                 method) {

  if(method == "steen") {

    ### Stop function if columns in data frame lack these specific names
    assertable::assert_colnames(data = df_sat,
                                colnames = c("time",
                                             "spec"),
                                only_colnames = FALSE,
                                quiet = TRUE)

    lm.intercept <- coef(std_obj$std_obj$std_lm_fit)[1]
    lm.slope <- coef(std_obj$std_obj$std_lm_fit)[2]

    df_sat$spec.to.std = (df_sat$spec - lm.intercept) / lm.slope

    df_sat_act <- df_sat %>%
      group_by(sub.conc) %>%
      mutate(activity = coef(lm(spec.to.std ~ time))[2])
  }

  if(method == "german") {

    lm.homo.slope <- coef(std_obj$std_obj$std_lm_fit_homo)[2]
    lm.buffer.slope <- coef(std_obj$std_obj$std_lm_fit_buffer)[2]

    df_sat$emission.coef <- lm.homo.slope / df_sat$assay.vol
    df_sat$quench.coef <- lm.homo.slope / lm.buffer.slope

    df_sat_act <- df_sat %>%
      dplyr::mutate(net.spec = ((spec - homo.control) / quench.coef) - sub.control) %>%
      dplyr::mutate(activity = (net.spec * buffer.vol) /
                      (emission.coef * homo.vol * time * soil.mass))
  }

  out_list <- list(act_calibrated_data = df_sat_act)

  out_list
}
