########
### Create new plot method for standard curve
########

plot.ezmmek_s_std_curve <- function(df) {

  ### Call previously named units
  conc.units <- df$s_std_curve_units[1]
  spec.units <- df$s_std_curve_units[2]

  ### Create standard curve plot
  s_std_curve_plot <- ggplot2::ggplot(data = df$s_std_curve_data, mapping = ggplot2::aes(x = std.conc, y = spec)) +
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
