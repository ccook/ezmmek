#' ezmmek_calc_mm_fit
#'
#' @description Calculate Michaelis-Menten fit
#'
#' @param df Dataframe of class 'new_ezmmek_calibrate'
#' @param km Starting value to estimate km. Default value is median of 'sub.conc' values
#' @param vmax Starting value to estimate vmax. Default value is max activity calculated

########
### Calculate nls fit model
########
ezmmek_calc_mm_fit <- function(df,
                               km,
                               vmax) {

  ### If statements to adjust column names
  if("act_calibrated_data_g" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act_calibrated_data = act_calibrated_data_g)
  }

  if("act_calibrated_data_s" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act_calibrated_data = act_calibrated_data_s)
  }

  ### Michaelis-Menten formula
  mm_form <- formula(activity_m ~ (vmax * substrate_conc) /
                       (km + substrate_conc))

  ### Assign starting values to predict km and vmax
  max_activity_m <- max(df$act_calibrated_data[[1]][[8]])
  median_substrate_conc <- median(df$act_calibrated_data[[1]][[8]])

  ### If km and vmax arguments are NULL, predict km and vmax values
  if(is.null(km) | is.null(vmax)) {

    mm_fit <- nls2::nls2(formula = mm_form, data = df$act_calibrated_data[[1]],
                         start = list(vmax = max_activity_m, km = median_substrate_conc))

    ### Else rely on user defined km and vmax
  } else {

    ### Michaelis-Menten formula
    mm_fit <- nls2::nls2(formula = mm_form, data = df$act_calibrated_data[[1]],
                         start = list(vmax = vmax, km = km))
  }

  ### Create a 1-column data frame with a 'grid' of points to predict
  min_substrate_conc <- min(df$act_calibrated_data[[1]][[1]])
  max_substrate_conc <- max(df$act_calibrated_data[[1]][[1]])
  pred_grid <- data.frame(substrate_conc = seq(from = min_substrate_conc, to = max_substrate_conc, length.out = 1000))

  out_list <- list(mm_fit = mm_fit,
                   pred_grid = pred_grid)

  out_list

}
