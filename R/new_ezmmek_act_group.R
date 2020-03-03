########
### Group raw activity data
########

new_ezmmek_act_group <- function(act.data.fn,
                                 ...,
                                 method = NA,
                                 columns = NULL) {


  # ### Stop function if method is not assigned approriately
  # if(
  #   !(method == "steen") & !(method == "german")
  # ) {
  #   stop("method must equal to 'steen' or 'german'")
  # }

  ### Read in data
  act_data <- read.csv(act.data.fn)

  if(is.null(columns)) {
    columns <- map_chr(enquos(...), rlang::quo_name)
  }

  act_data_grouped <- act_data %>%
    group_by_at(vars(intersect(names(.), columns))) %>%
  nest(.key = "activity.data")

  class(act_data_grouped) <- c("new_ezmmek_act_group", "data.frame")

  act_data_grouped

}
