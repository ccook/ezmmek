#' ezmmek_calc_std_lm_homo_buffer
#'
#' @description Calculate linear model for standard curve in homogenate-buffer solution
#'
#' @param df Standard curve dataframe

########
### Make standard curve lm object for homogenate-buffer solution
########

ezmmek_calc_std_lm_homo_buffer <- function(df) {

  ### Fit linear model to homogenate
  std_curve_lm_homo_buffer <- lm(formula = homo_buffer_signal ~ std_conc, data = df)

  std_curve_lm_homo_buffer
}
