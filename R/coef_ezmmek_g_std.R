########
### Create new coef method for standard curve
########

coef_ezmmek_g_std <- function(g_std_obj) {

  ### Call buffer linear model intercept
  buffer.lm.intercept <- coef(g_std_obj$g_std_lm_fit_buffer)[1]

  ### Call buffer linear model slope
  buffer.lm.slope <- coef(g_std_obj$g_std_lm_fit_buffer)[2]

  ### Call homogenate linear model intercept
  homo.lm.intercept <- coef(g_std_obj$g_std_lm_fit_homo)[1]

  ### Call homogenate linear model intercept
  homo.lm.slope <- coef(g_std_obj$g_std_lm_fit_homo)[2]

  ### Print coefs
  print(buffer.lm.intercept)
  print(buffer.lm.slope)
  print(homo.lm.intercept)
  print(homo.lm.slope)

  ### Output list containing intercept and slope
  out_list <- list(g_std_curve_lm_buffer_intercept = buffer.lm.intercept,
                   g_std_curve_lm_buffer_slope = buffer.lm.slope,
                   g_std_curve_lm_homo_intercept = homo.lm.intercept,
                   g_std_curve_lm_homo_slope = homo.lm.slope)

  }
