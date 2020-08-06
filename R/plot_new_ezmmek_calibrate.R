##' plot_new_ezmmek_calibrate
##'
##' @export
##'
##' @description Plots new_ezmmek_calibrate object and facets by specified column names
##'
##' @param x data.frame object of class new_ezmmek_calibrate
##' @param ... User defined column names by which to facet plot
##'
##' @examples
##' \dontrun{plot.new_ezmmek_act_group(new_ezmmek_calibrate_obj,
##' site_name,
##' std_type)}

plot.new_ezmmek_calibrate <- function(x, ...) {

  ### User-defined columns to facet by if column names not supplied by parent fxn
  if(is.null(columns)) {
    columns <- rlang::enquos(...)
  }


  ### Correct for different column names with 'if' statements
  ### German protocol
  if("act_calibrated_data_ibc" %in% colnames(x)) {
    df <- x %>% dplyr::rename(act_calibrated_data = act_calibrated_data_ibc,
                               std_raw_data = std_raw_data_ibc)
  }

  ### Steen protocol
  if("act_calibrated_data_isc" %in% colnames(x)) {
    df <- x %>% dplyr::rename(act_calibrated_data = act_calibrated_data_isc,
                               std_raw_data = std_raw_data_isc)
  }

  ### Unnest activity data
  unnest_cal_df <- tidyr::unnest(df, act_calibrated_data)

  ### Make plot of activity data
  cal_plot <- ggplot2::ggplot(data = unnest_cal_df,
                              mapping = ggplot2::aes(x = substrate_conc,
                                                     y = activity_m)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = activity_m - activity_sd,
                                        ymax = activity_m + activity_sd)) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(columns)

cal_plot

}
