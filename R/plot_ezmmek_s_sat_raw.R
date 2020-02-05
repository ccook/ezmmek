########
### Create new plot method for raw data
########

plot.ezmmek_s_raw_data <- function(df) {

  ### Call previously named units
  time.units <- df$s_raw_units[1]
  spec.units <- df$s_raw_units[2]

  ### Create plot of raw data
  sat_raw_plot <- ggplot2::ggplot(data = df$s_raw_data,
                                  mapping = ggplot2::aes(
                                    x = time,
                                    y = spec,
                                    color = as.factor(replicate))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::facet_wrap(~sub.conc, scales = "fixed") +
    ggplot2::theme(axis.text.y = ggplot2::element_text()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text()) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::scale_color_discrete(name = "replicate") +
    ggplot2::theme_bw() +
    ggplot2::xlab(time.units) +
    ggplot2::ylab(spec.units)

  ### Output list containing plot
  out_list <- list(sat_raw_plot = sat_raw_plot)

}
