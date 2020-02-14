########
### Create new summary method for standard curve
########

summary.ezmmek_s_std <- function(s_std_obj) {

  ### Create linear model summary of lm object
  s_std_curve_lm_summary <- summary(s_std_obj$s_std_lm_fit)

  ### Print lm summary
  print(s_std_curve_lm_summary)

  ### Output list containing summary
  out_list <- list(s_std_curve_lm_summary = s_std_curve_lm_summary)

}
