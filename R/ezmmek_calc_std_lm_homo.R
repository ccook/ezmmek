#' ezmmek_calc_std_lm_homo
#'
#' @description Calculate linear model for standard curve in homogenate
#'
#' @param df Standard curve dataframe

########
### Make standard curve lm object for homogenate
########

ezmmek_calc_std_lm_homo <- function(df) {

  ### Fit linear model to homogenate
  std_curve_lm_homo <- lm(formula = homo_signal ~ std_conc, data = df)

  std_curve_lm_homo
}
