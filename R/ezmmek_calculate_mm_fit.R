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
ezmmek_calc_mm_fit <- function(df, km, vmax) {

  ### Assign starting values to predict km and vmax
  max.activity.m <- max(df$activity.m)
  median.sub.conc <- median(df$sub.conc)

  ### Michaelis-Menten formula
  mm_form <- formula(df$activity.m ~ (vmax * df$sub.conc) /
                       (km + df$sub.conc))

  ### If km and vmax arguments are NULL, predict km and vmax values
  if(is.null(km) | is.null(vmax)) {

  mm_fit <- nls2::nls2(formula = mm_form, data = df,
                 start = list(vmax = max.activity.m, km = median.sub.conc))

  ### Else rely on user defined km and vmax
  } else {

    mm_fit <- nls2::nls2(formula = mm_form, data = df,
                   start = list(vmax = vmax, km = km))
  }


  mm_fit

}
