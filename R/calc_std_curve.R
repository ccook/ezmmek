##' Calculates a standard curve
##' @export # Not sure what you actyally want to export
##'
##'
calc_std_curve <- function(df) {

  # Need to wrap this in tryCatch
  mod <- ezmmek_lm(data = df)
  attr(mod, "class") <- "std_curve" # arguably name it something more specific
  mod
}
