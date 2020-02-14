########
### Create new coef method for saturation curve fit
########

coef.ezmmek_s_sat <- function(s_sat_obj) {

  ### Call mm model vmax
  mm.vmax <- coef(s_sat_obj$s_sat_mm_fit)[1]

  ### Call mm model km
  mm.km <- coef(s_sat_obj$s_sat_mm_fit)[2]

  ### Print coefs
  print(mm.vmax)
  print(mm.km)

  ### Output list containing vmax and km
  out_list <- list(s_sat_curve_mm_vmax = mm.vmax,
                   s_sat_curve_mm_km = mm.km)

}
