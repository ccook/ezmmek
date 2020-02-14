########
### Create new coef method for standard curve
########

coef.ezmmek_s_std <- function(s_std_obj) {

  ### Call linear model intercept
  lm.intercept <- coef(s_std_obj$s_std_lm_fit)[1]

  ### Call linear model slope
  lm.slope <- coef(s_std_obj$s_std_lm_fit)[2]

  ### Print coefs
  print(lm.intercept)
  print(lm.slope)

  ### Output list containing intercept and slope
  out_list <- list(s_std_curve_lm_intercept = lm.intercept,
                   s_std_curve_lm_slope = lm.slope)

}
