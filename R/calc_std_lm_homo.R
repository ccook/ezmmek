########
### Make standard curve lm object for homogenate
########

calc_std_lm_homo <- function(df) {

  ### Fit linear model to homogenate
  std_curve_lm_homo <- lm(formula = homo.signal ~ std.conc, data = df)

  std_curve_lm_homo
}
