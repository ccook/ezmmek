########
### Create new coef method for standard curve
########

coef.ezmmek_s_std_curve <- function(mod) {

  ### Call linear model intercept
  lm.intercept <- coef(mod$s_std_curve_lm_fit)[1]

  ### Call linear model slope
  lm.slope <- coef(mod$s_std_curve_fit_lm_fit)[2]

  ### Output list containing intercept and slope
  out_list <- list(s_std_curve_lm_intercept = lm.intercept, s_std_curve_lm_slope = lm.slope)

}
