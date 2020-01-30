##' Create lms for ezmmek package w/ safe failure modes
##' # Do not export

ezmmek_lm <- function(df) {

  if(nrow(df) == 1) {
    # add a row of all zeroes to the data - not sure best way to do this right now.
  }

  # Tries to create a lm from thje data, returns null if it can't (gotta check what it retursn if there is a warning)
  model <- tryCatch(
    lm(value ~ conc, data = std_data),
    warning = function(w) warning(w),
    error = function(e) {
      warning("An lm did not work correctly")
      NULL # This returns null if the lm cannot be calculated
    }
  )

  model
}

