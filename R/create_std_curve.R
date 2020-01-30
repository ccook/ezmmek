##' Reads and calculates a standard curve
##' @export
##'
create_std_curves <- function(fn, conc.units = 'units not defined', ...) {
  # put argument checks here

  # Read the data file
  std_data <- readr::read_csv(fn)
  attr(std_data, "conc.units") <- conc.units

  # Later we should set this up so that it will make multiple std curves (eg for each site, fluorophore, etc)

  # Create the linear model
  std_curve_mod <- ezmmek_lm(data = std_data)




}

