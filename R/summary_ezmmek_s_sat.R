########
### Create new summary method for saturation curve fit
########

summary.ezmmek_s_sat <- function(s_sat_obj) {

  ### Create model summary of mm object
  s_sat_curve_mm_summary <- summary(s_sat_obj$s_sat_mm_fit)

  ### Print mm summary
  print(s_sat_curve_mm_summary)

  ### Output list containing summary
  out_list <- list(s_sat_curve_mm_summary = s_sat_curve_mm_summary)

}
