#' new_ezmmek_sat_fit
#'
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @description Creates dataframe containing calibrated enzyme activity data and Michaelis-Menton fit of class 'new_ezmmek_sat_fit'
#'
#' @param std.data.fn Standard data file as character string
#' @param act.data.fn Activity data file as character string
#' @param ... User defined column names to join and group std.data.fn and act.data.fn
#' @param km Starting value to estimate km. Default value is median of 'sub.conc' values
#' @param vmax Starting value to estimate vmax. Default value is max activity calculated
#' @param method Enzyme assay protocol. Must define method as 'steen' or 'german'
#'
#' @examples
#' new_ezmmek_sat_fit(std.data.fn, act.data.fn, site.name, std.type, km = NULL, vmax = NULL, method = "steen")

########
### Calculate Michaelis-Menten fit and add to dataframe
########

new_ezmmek_sat_fit <- function(std.data.fn,
                               act.data.fn,
                               ...,
                               km = NULL,
                               vmax = NULL,
                               method = NA) {

  ### User names columns to be grouped
  columns <- purrr::map_chr(rlang::enquos(...), rlang::quo_name)

  ### Calibrate and calculate activities
  calibrated_df <- new_ezmmek_act_calibrate(std.data.fn,
                                            act.data.fn,
                                            ...,
                                            method = method,
                                            columns = columns)

  ### Group data frame by substrate type and the additional arguments put in by user
  calibrated_df_grouped <- calibrated_df %>%
    dplyr::group_by_at(dplyr::vars(sub.type, intersect(names(.), columns))) %>%
    tidyr::nest()

  ### Creates new Michaelis-Menten fit columns
  calibrated_df_mm_fit <- calibrated_df_grouped %>%
    dplyr::mutate(mm.fit.obj = purrr::map(data, function(df) ezmmek_calc_mm_fit(df, km, vmax) %>% purrr::pluck(1)), #nlsm
                  km = purrr::map_dbl(data, function(df) coef(ezmmek_calc_mm_fit(df, km, vmax) %>% purrr::pluck(1))[2]), #km
                  vmax = purrr::map_dbl(data, function(df) coef(ezmmek_calc_mm_fit(df, km, vmax) %>% purrr::pluck(1))[1]), #vmax
                  pred_grid = purrr::map(data, function(df) ezmmek_calc_mm_fit(df, km, vmax) %>% purrr::pluck(2))) %>%
    tidyr::unnest(data)

  ### Function to apply mm_fit to each value in pred_grid
  predict_df <- function(mm_fit, pred_grid) {
    pred.vec <- predict(mm_fit, pred_grid)
    pred_df <- data.frame(sub.conc = pred_grid$sub.conc, activity.m = pred.vec)
    pred_df
  }

  ### Apply predict_df() to pred_grid in each row
  result_df <- calibrated_df_mm_fit %>%
    dplyr::mutate(pred_activities = purrr::map2(.x = mm.fit.obj, .y = pred_grid, .f = predict_df))

  ### Assign new class
  class(result_df) <- c("new_ezmmek_sat_fit", "data.frame")

  result_df
}
