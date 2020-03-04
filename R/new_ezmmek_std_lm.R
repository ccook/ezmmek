########
### Make standard curve lm object
########

new_ezmmek_std_lm <- function(df,
                              columns,
                              method,
                              conc.units = NA,
                              signal.units = NA) {

  ### Stop function if method is not assigned approriately
  if(
    !(method == "steen") & !(method == "german")
  ) {
    stop("method must equal 'steen' or 'german'")
  }

  if("std.conc" %in% columns) {
    stop("Cannot group arguments used to calculate linear model ('std.conc', 'homo.signal', 'buffer.signal')")
  }

  ### Steen method
  if(method == "steen") {

    ### Require certain column names
    assert_colnames(data = df,
                    colnames = c("std.conc",
                                 "homo.signal"),
                    only_colnames = FALSE,
                    quiet = TRUE)

  ###### Groups data by user-decided column names
  ######### Creates dataframe containing lm list for each unique set of grouped column
  std_data_lm <- df %>%
    group_by_at(vars(intersect(names(.), columns))) %>%
    group_nest(.key = "std.raw.data.s") %>%
    mutate(std.lm.homo.obj = map(std.raw.data.s, function(df) calc_std_lm_homo(df)), #homogenate lm
    std.lm.homo.slope = map_dbl(std.raw.data.s, function(df) coef(calc_std_lm_homo(df))[2]), #homogenate slope
    std.lm.homo.intercept = map_dbl(std.raw.data.s, function(df) coef(calc_std_lm_homo(df))[1]) #homogenate intercept
    )
}

### German method
if(method == "german") {

  ### Require certain column names
  assert_colnames(data = df,
                  colnames = c("std.conc",
                               "homo.signal",
                               "buffer.signal"),
                  only_colnames = FALSE,
                  quiet = TRUE)

  ###### Groups data by user-decided column names
  ######### Creates dataframe containing lm list for each unique set of grouped column
  std_data_lm <- df %>%
    group_by_at(vars(intersect(names(.), columns))) %>%
    group_nest(.key = "std.raw.data.g") %>%
    mutate(std.lm.homo.obj = map(std.raw.data.g, function(df) calc_std_lm_homo(df)), #homogenate lm
           std.lm.homo.slope = map_dbl(std.raw.data.g, function(df) coef(calc_std_lm_homo(df))[2]), #homogenate slope
           std.lm.homo.intercept = map_dbl(std.raw.data.g, function(df) coef(calc_std_lm_homo(df))[1]), #homogenate intercept
           std.lm.buffer.obj = map(std.raw.data.g, function(df) calc_std_lm_buffer(df)), #buffer lm
           std.lm.buffer.slope = map_dbl(std.raw.data.g, function(df) coef(calc_std_lm_buffer(df))[2]), #buffer slope
           std.lm.buffer.intercept = map_dbl(std.raw.data.g, function(df) coef(calc_std_lm_buffer(df))[1]), #buffer intercept
           quench.coef = std.lm.homo.slope / std.lm.buffer.slope #quench coefficient
    )
}

units <- c("conc.units" = conc.units,
           "signal.units" = signal.units)

class(std_data_lm) <- c("new_ezmmek_std_lm", "data.frame")
attr(std_data_lm, "units") <- units

std_data_lm

}
