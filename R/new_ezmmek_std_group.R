########
### Group standard lm objects
########

new_ezmmek_std_group <- function(std.data.fn,
                                 ...,
                                 method = NA,
                                 conc.units = NA,
                                 signal.units = NA,
                                 columns = NULL) {

  # ### Stop function if method is not assigned approriately
  # if(
  #   !(method == "steen") & !(method == "german")
  # ) {
  #   stop("method must equal to 'steen' or 'german'")
  # }

  ### Read in data
  std_data <- read.csv(std.data.fn)

  if(is.null(columns)) {
    columns <- map_chr(enquos(...), rlang::quo_name)
  }


  std_data_grouped <- new_ezmmek_std_lm(std_data,
                                        columns,
                                        method,
                                        conc.units,
                                        signal.units)

  ### Assign new class
  class(std_data_grouped) <- c("new_ezmmek_std_group", "data.frame")

  std_data_grouped

}

