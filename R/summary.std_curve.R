##' Summary method for objects of class std_curve
##' # Do not export (I think)
##'
##'
summary.std_curve <- function(x) {
  print("This is a standard curve object")
  attr(x, "class") <- "lm" # This is super janky: I turned the lm object into an object of class "std_curve", but now I'm turning it back into an object of class "lm" and just printing the regular lm summary
  print(summary(x))
}
