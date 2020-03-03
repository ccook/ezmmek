########
### Make standard curve lm object for bufffer
########

calc_std_lm_buffer <- function(df) {

  ### Fit linear model to buffer
  std_curve_lm_buffer <- lm(formula = buffer.signal ~ std.conc, data = df)

  std_curve_lm_buffer

}
