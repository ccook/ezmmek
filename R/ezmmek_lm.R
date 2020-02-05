
ezmmek_lm <- function(df) {

  UseMethod("ezmmek_lm")
}

  ezmmek_lm.data.frame <- function(df){

  ### Stop function if columns lack these specific names
  assertable::assert_colnames(data = df,
                              colnames = c("std.conc", "spec"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Create linear model
  lm_std_curve <- lm(formula = spec ~ std.conc, data = df)
  summary(lm_std_curve)

  lm_intercept <- coef(lm_std_curve)[1]
  lm_slope <- coef(lm_std_curve)[2]



  out_list <- list(std_fit = lm_std_curve, std_intercept = lm_intercept, std_slope = lm_slope, raw_data=df)
  out_list
}

