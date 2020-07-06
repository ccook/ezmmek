#' new_ezmmek_act_calibrate
#'
#' @export
#'
#' @description Creates dataframe containing calibrated enzyme activity data of class 'new_ezmmek_calibrate'
#'
#' @param std.data.fn Standard data file as character string
#' @param act.data.fn Activity data file as character string
#' @param ... User defined column names to join std.data.fn and act.data.fn
#' @param method Enzyme assay protocol. Must define method as 'steen' or 'german'
#'
#' @examples
#' new_ezmmek_act_calibrate("data/tyson_std_04172020.csv", "data/tyson_sat_steen_04172020.csv", site.name, std.type, km = NULL, vmax = NULL, method = "isc")
#' new_ezmmek_act_calibrate("data/tyson_std_04172020.csv", "data/tyson_sat_german_04172020.csv", site.name, std.type, km = NULL, vmax = NULL, method = "ibc")

########
### Join activity dataframe with standard dataframe and calibrate
########

new_ezmmek_act_calibrate <- function(std.data.fn,
                                     act.data.fn,
                                     ...,
                                     method = NA,
                                     columns = NULL) {

  ### Use '...' arguments if column names not supplied in parent fxn
  if(is.null(columns)) {
    columns <- purrr::map_chr(rlang::enquos(...), rlang::quo_name)
  }
  ### Creates dataframe of standard curve data
  std_data_grouped <- new_ezmmek_std_group(std.data.fn,
                                           method = method,
                                           columns = columns)

  ### Creates dataframe of raw activity data
  act_data_grouped <- new_ezmmek_act_group(act.data.fn,
                                           method = method,
                                           columns = columns)

  ### Joins the two data frames based on common descriptor columns
  std_act_std <- dplyr::full_join(act_data_grouped, std_data_grouped)

  ### Calibrate activities
  std_act_calibrated <- ezmmek_calibrate_activities(std_act_std, method, columns)

   ### Assign new class
  class(std_act_calibrated) <- c("new_ezmmek_calibrate", "data.frame")

  std_act_calibrated

}
