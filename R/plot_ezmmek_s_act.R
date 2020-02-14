########
### Create new plot method for raw activity data
########

plot.ezmmek_s_act <- function(s_act_obj) {

  ### Call previously named units
  time.units <- s_act_obj$s_act_units[1]
  spec.units <- s_act_obj$s_act_units[2]

  ### Create plot of raw data
  s_act_plot <- ggplot2::ggplot(data = s_act_obj$s_act_data,
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

  print(s_act_plot)

  ### Output list containing plot
  out_list <- list(s_act_plot = s_act_plot)

}
