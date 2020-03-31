######## Generate lm for mtcars

mtcars_calc <- function(df) {

  mtcars_lm <- lm(mpg ~ wt, data = mtcars)

  min.mpg <- min(df$mpg)
  max.mpg <- max(df$mpg)
  pred_grid <- data.frame(mpg = min.mpg:max.mpg)

  out_list <- list(mtcars_lm = mtcars_lm,
                   pred_grid = pred_grid)

  out_list
}
