########
### Create new plot method for standard curve
########

plot.ezmmek_s_std <- function(s_std_obj) {

  ### Call previously named units
  conc.units <- s_std_obj$s_std_units[1]
  spec.units <- s_std_obj$s_std_units[2]

  ### Create standard curve plot
  s_std_curve_plot <- ggplot2::ggplot(data = s_std_obj$s_std_data, mapping = ggplot2::aes(x = std.conc, y = spec)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::xlab(conc.units) +
    ggplot2::ylab(spec.units)

  ### Print plot
  print(s_std_curve_plot)

  ### Output list containing plot
  out_list <- list(s_std_curve_plot = s_std_curve_plot)

}
