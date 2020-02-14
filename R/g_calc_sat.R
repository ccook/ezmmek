##' @export

g_calc_sat <- function(df_std,
                       df_sat,
                       assay.vol = 1,
                       sat.rate.units = NULL,
                       sat.sub.conc.units = NULL,
                       km = NULL,
                       vmax = NULL,
                       act.time.units = NULL,
                       act.spec.units = NULL,
                       std.conc.units = NULL,
                       std.spec.units = NULL,
                       ...) {

  ### Run function that creates standard curve object
  std_obj <- g_calc_std(df_std,
                        std.conc.units,
                        std.spec.units,
                        ...)

  ### Run function that creates activity object
  act_obj <- g_calc_act(df_sat,
                        act.time.units,
                        act.spec.units,
                        df_std = df_std,
                        ...)

  ### Create linear model for standard curve in homogenate
  lm_fit_homo <- lm(spec.homo ~ std.conc, data = df_std)

  ### Create linear model for standard curve in buffer
  lm_fit_buffer <- lm(spec.buffer ~ std.conc, data = df_std)

  ### Call slopes of both linear models
  lm_fit_homo.slope <- coef(lm_fit_homo)[2]
  lm_fit_buffer.slope <- coef(lm_fit_buffer)[2]

  ### Calculate emission and quench coefs
  emission.coef <- lm_fit_homo.slope / assay.vol
  quench.coef <- lm_fit_homo.slope / lm_fit_buffer.slope

  ### Create new dataframe containing calculated activities
  df_sat_activity <- act_obj$g_act_data %>%
    dplyr::group_by(sub.conc, replicate) %>%
    dplyr::mutate(net.spec = ((spec - homo.control) / quench.coef) - sub.control) %>%
    dplyr::mutate(activity = (net.spec * buffer.vol) /
                    (emission.coef * homo.vol * time * sample.unit)) %>%
    dplyr::group_by(sub.conc) %>%
    dplyr::mutate(activity.m = mean(activity), activity.sd = sd(activity))

  ### Assign starting values to predict km and vmax
  max.activity.m <- max(df_sat_activity$activity.m)
  median.conc <- median(df_sat_activity$sub.conc)

  mm_form <- formula(activity.m ~ (vmax * sub.conc) /
                       (km + sub.conc))

  ### If km and vmax arguments are NULL, predict km and vmax values
  if(is.null(km) | is.null(vmax)) {

    mm_fit <- nls2::nls2(formula = mm_form, data = df_sat_activity,
                         start = list(vmax = max.activity.m, km = median.conc))

  } else {

    ### Else rely on user defined km and vmax
    mm_fit <- nls2::nls2(formula = mm_form, data = df_sat_activity,
                         start = list(vmax = vmax, km = km))

  }

  ### Create a 1-column data frame with a grid of points to predict
  min.sub.conc <- min(df_sat_activity$sub.conc)
  max.sub.conc <- max(df_sat_activity$sub.conc)
  pred_grid <- data.frame(sub.conc = min.sub.conc:max.sub.conc)

  ### Put the predicted values into a data frame, paired with the values at which they were predicted
  predictions <- predict(mm_fit, newdata = pred_grid)

  pred_df <- data.frame(sub.conc = pred_grid$sub.conc, activity.m = predictions)

  ### Create vector of units
  units <- c("sat.rate.units" = sat.rate.units,
             "sat.sub.conc.units" = sat.sub.conc.units)


  out_list <- list(g_sat_activity_data = df_sat_activity,
                   g_sat_units = units,
                   g_sat_mm_fit = mm_fit,
                   g_sat_pred_act = pred_df,
                   g_std_obj = std_obj,
                   g_act_obj = act_obj,
                   ...)

  class(out_list) <- c("ezmmek_g_sat", "list")

  out_list

}
