########
### Make standard curve lm object
########

new_ezmmek_std_lm <- function(df,
                              columns,
                              method = NA,
                              conc.units = NA,
                              signal.units = NA) {
  #
  #   ### Steen method
  #   if(method == "steen") {

  ###### Groups data by user-decided column names
  ######### Creates dataframe containing lm list for each unique set of grouped column
  std_data_lm <- df %>%
    group_by_at(vars(intersect(names(.), columns))) %>%
    nest(.key = "std.data") %>%
    mutate(std.lm.homo.obj = map(std.data, function(df) calc_std_lm_homo(df)), #homogenate lm
    std.lm.homo.slope = map_dbl(std.data, function(df) coef(calc_std_lm_homo(df))[2]), #homogenate slope
    std.lm.homo.intercept = map_dbl(std.data, function(df) coef(calc_std_lm_homo(df))[1]) #homogenate intercept
    )

  # std_data_lm <- df %>%
  #   group_by(!!! columns) %>%
  #   nest() %>%
  #   mutate(std.lm.homo.obj = map(data, function(df) calc_std_lm_homo(df)), #homogenate lm
  #          std.lm.homo.slope = map_dbl(data, function(df) coef(calc_std_lm_homo(df))[2]), #homogenate slope
  #          std.lm.homo.intercept = map_dbl(data, function(df) coef(calc_std_lm_homo(df))[1]) #homogenate intercept
  #   )
#}

# ### German method
# if(method == "german") {
#
#   ###### Groups data by user-decided column names
#   ######### Creates dataframe containing lm list for each unique set of grouped column
#   std_data_lm <- df %>%
#     group_by(!!! columns) %>%
#     nest() %>%
#     mutate(std.lm.homo.obj = map(data, function(df) calc_std_lm_homo(df)), #homogenate lm
#            std.lm.homo.slope = map_dbl(data, function(df) coef(calc_std_lm_homo(df))[2]), #homogenate slope
#            std.lm.homo.intercept = map_dbl(data, function(df) coef(calc_std_lm_homo(df))[1]), #homogenate intercept
#            std.lm.buffer.obj = map(data, function(df) calc_std_lm_buffer(df)), #buffer lm
#            std.lm.buffer.slope = map_dbl(data, function(df) coef(new_ezmmek_std_lm_buffer(df))[2]), #buffer slope
#            std.lm.buffer.intercept = map_dbl(data, function(df) coef(calc_std_lm_buffer(df))[1]), #buffer intercept
#            quench.coef = std.lm.homo.slope / std.lm.buffer.slope #quench coefficient
#     )
# }

units <- c("conc.units" = conc.units,
           "signal.units" = signal.units)

class(std_data_lm) <- c("new_ezmmek_std_lm", "data.frame")
attr(std_data_lm, "units") <- units

std_data_lm

}
