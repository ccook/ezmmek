#' ezmmek_calc_std_lm_homo
#'
#' @description Calculate linear model for standard curve in homogenate
#'
#' @param df Standard curve dataframe

########
### Make standard curve lm object for buffer
########

ezmmek_calc_std_lm_buffer <- function(df) {

  ### Fit linear model to buffer
  std_curve_lm_buffer <- lm(formula = buffer_signal ~ std_conc, data = df)

  std_curve_lm_buffer

}
