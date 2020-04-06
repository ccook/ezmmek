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

  if("act.calibrated.data.g" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act.calibrated.data = act.calibrated.data.g)
  }

  if("act.calibrated.data.s" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act.calibrated.data = act.calibrated.data.s)
  }

  ### Michaelis-Menten formula
  mm_form <- formula(activity.m ~ (vmax * sub.conc) /
                       (km + sub.conc))

  ### Assign starting values to predict km and vmax
  max.activity.m <- max(df$act.calibrated.data[[1]][[8]])
  median.sub.conc <- median(df$act.calibrated.data[[1]][[8]])

  ### If km and vmax arguments are NULL, predict km and vmax values
  if(is.null(km) | is.null(vmax)) {

    mm_fit <- nls2::nls2(formula = mm_form, data = df$act.calibrated.data[[1]],
                         start = list(vmax = max.activity.m, km = median.sub.conc))

    ### Else rely on user defined km and vmax
  } else {

    ### Michaelis-Menten formula
    mm_fit <- nls2::nls2(formula = mm_form, data = df$act.calibrated.data[[1]],
                         start = list(vmax = vmax, km = km))
  }

  ### Create a 1-column data frame with a 'grid' of points to predict
  min.sub.conc <- min(df$act.calibrated.data[[1]][[1]])
  max.sub.conc <- max(df$act.calibrated.data[[1]][[1]])
  pred_grid <- data.frame(sub.conc = seq(from = min.sub.conc, to = max.sub.conc, length.out = 1000))

  out_list <- list(mm_fit = mm_fit,
                   pred_grid = pred_grid)

  out_list

}
