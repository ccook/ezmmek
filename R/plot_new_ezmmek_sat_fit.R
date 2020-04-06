##' @export

plot.new_ezmmek_sat_fit <- function(df, ...) {

  ### User-defined columns to facet by
  columns <- rlang::enquos(...)

  ### Plot points without curve fit
  point_plot <- plot.new_ezmmek_calibrate(df, columns = columns)

  ### Unnest predicted activities df
  unnest_sat_df <- tidyr::unnest(df, pred_activities)

  sat_fit_plot <- point_plot +
    ggplot2::geom_line(data = unnest_sat_df,
                       ggplot2::aes(x = sub.conc, y = activity.m)) +
    ggplot2::facet_wrap(columns)

  sat_fit_plot

}
