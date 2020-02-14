########
### Create new plot method for saturation curve

plot.ezmmek_s_sat <- function(s_sat_obj) {

  ### Call previously assigned units
  rate.units <- s_sat_obj$s_sat_units[1]
  sub.conc.units <- s_sat_obj$s_sat_units[2]

  ### Create plot of activities without fitted curves
  s_sat_scatter <- ggplot2::ggplot(data = s_sat_obj$s_sat_activity_data,
                                   mapping = ggplot2::aes(x = sub.conc,
                                                          y = activity.m)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = activity.m - activity.sd,
                                        ymax = activity.m + activity.sd)) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::xlab(sub.conc.units) +
    ggplot2::ylab(rate.units)

  ### Print scatter plot
  print(s_sat_scatter)

  ### Create and print plot of saturation curve with fit
  s_sat_curve <- s_sat_scatter +
    ggplot2::geom_line(data = s_sat_obj$s_sat_pred_act, ggplot2::aes(x = sub.conc, y = activity.m))

  print(s_sat_curve)

  out_list <- list(s_sat_curve = s_sat_curve,
                   s_sat_scatter = s_sat_scatter)

}
