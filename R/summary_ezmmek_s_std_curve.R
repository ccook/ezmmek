########
### Create new summary method for standard curve
########

summary.ezmmek_s_std_curve <- function(mod) {

  ### Create linear model summary of lm object
  s_std_curve_lm_summary <- summary(mod$s_std_curve_fit)

  ### Output list containing summary
  out_list <- list(s_std_curve_lm_summary = s_std_curve_lm_summary)

}
