########
### Group standard lm objects
########

new_ezmmek_std_group <- function(std.data.fn,
                                 ...,
                                 method = NA,
                                 conc.units = NA,
                                 signal.units = NA,
                                 columns = NULL) {

  ### Read in data
  std_data <- read.csv(std.data.fn)

  ### Use '...' arguments if column names not supplied in parent fxn, new_ezmmek_calibrate
  if(is.null(columns)) {
    columns <- map_chr(enquos(...), rlang::quo_name)
  }

### Group standard data
  std_data_grouped <- new_ezmmek_std_lm(std_data,
                                        columns,
                                        method,
                                        conc.units,
                                        signal.units)

  units <- c("conc.units" = conc.units,
             "signal.units" = signal.units)

  ### Assign new class
  class(std_data_grouped) <- c("new_ezmmek_std_group", "data.frame")
  attr(std_data_grouped, "units") <- units

  std_data_grouped

}

