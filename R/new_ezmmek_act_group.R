########
### Group raw activity data
########

new_ezmmek_act_group <- function(act.data.fn,
                                 ...,
                                 method = NA,
                                 columns = NULL) {

  ### Read in data
  act_data <- read.csv(act.data.fn)

  ### Use '...' arguments if column names not supplied in parent fxn, new_ezmmek_calibrate
  if(is.null(columns)) {
    columns <- map_chr(enquos(...), rlang::quo_name)
  }

  ### Steen method required column names
  if(method == "steen") {
    assert_colnames(data = act_data,
                    colnames = c("time",
                                 "signal",
                                 "sub.conc"),
                    only_colnames = FALSE,
                    quiet = TRUE)


  act_data_grouped <- act_data %>%
    group_by_at(vars(intersect(names(.), columns))) %>%
    group_nest(.key = "act.raw.data.s")
  }

  if(method == "german") {
    assert_colnames(data = act_data,
                    colnames = c("time",
                                 "signal",
                                 "sub.conc",
                                 "buffer.vol",
                                 "homo.vol",
                                 "soil.mass",
                                 "assay.vol",
                                 "homo.control",
                                 "sub.control"),
                    only_colnames = FALSE,
                    quiet = TRUE)

    act_data_grouped <- act_data %>%
      group_by_at(vars(intersect(names(.), columns))) %>%
      group_nest(.key = "act.raw.data.g")
  }

  class(act_data_grouped) <- c("new_ezmmek_act_group", "data.frame")

  act_data_grouped

}
