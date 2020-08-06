##' plot_new_ezmmek_sat_fit
##'
##' @export
##'
##' @description Plots new_ezmmek_sat_fit object and facets by specified column names
##'
##' @param x data.frame object of class new_ezmmek_sat_fit
##' @param ... User defined column names by which to facet plot
##'
##' @examples
##' \dontrun{plot.new_ezmmek_act_group(new_ezmmek_sat_fit_obj,
##' site_name,
##' stdy_type)}

plot.new_ezmmek_sat_fit <- function(x, ...) {

  ### User-defined columns to facet by
  columns <- rlang::enquos(...)

  ### Plot points without curve fit
  point_plot <- plot.new_ezmmek_calibrate(x, columns = columns)

  ### Unnest predicted activities df
  unnest_sat_df <- tidyr::unnest(x, pred_activities)

  sat_fit_plot <- point_plot +
    ggplot2::geom_line(data = unnest_sat_df,
                       ggplot2::aes(x = substrate_conc, y = activity_m)) +
    ggplot2::facet_wrap(columns)

  sat_fit_plot

}
