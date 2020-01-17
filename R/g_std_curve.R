g_std_curve <- function(d_std_g, x.label = NULL, y.label = NULL) {


  ### Stop function if columns lack these specific names
  assertable::assert_colnames(data = d_std_g,
                              colnames = c("std.conc", "spec.homo", "spec.buffer", "replicate"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Add mean spec values of standard in buffer and standard in homogenate to new dataframe
  d_std_g_mean <- d_std_g %>%
    dplyr::group_by(std.conc) %>%
    dplyr::mutate(spec.m.homo = mean(spec.homo)) %>%
    dplyr::mutate(spec.m.buffer = mean(spec.buffer))

  ### Calculate slope of mean spec value of standard in homogenate
  lm_fit_homo <- lm(spec.m.homo ~ std.conc, data = d_std_g_mean)
  print(summary(lm_fit_homo))

  ### Calculate slope of mean spec value of standard in buffer
  lm_fit_buffer <- lm(spec.m.buffer ~ std.conc, data = d_std_g_mean)
  print(summary(lm_fit_buffer))

  x.label <- x.label
  x.label <- y.label

  g_std_curve_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = d_std_g_mean, ggplot2::aes(x = std.conc, y = spec.buffer, color = "blue")) +
    ggplot2::geom_point(data = d_std_g_mean, ggplot2::aes(x = std.conc, y = spec.homo, color = "red")) +
    ggplot2::theme_bw() +
    ggplot2::xlab(x.label) +
    ggplot2::ylab(y.label) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::theme(axis.text = ggplot2::element_text(), axis.title = ggplot2::element_text()) +
    ggplot2::geom_smooth(data = d_std_g_mean, ggplot2::aes(x = std.conc, y = spec.buffer), method = "lm", color = "blue") +
    ggplot2::geom_smooth(data = d_std_g_mean, ggplot2::aes(x = std.conc, y = spec.homo), method = "lm", color = "red") +
    ggplot2::scale_color_discrete(name = "Solution Type", labels = c("Buffer", "Homogenate"))

  print(g_std_curve_plot)

  outlist <- list(std_data = d_std_g_mean,
                  fit_buffer = lm_fit_buffer,
                  fit_homo = lm_fit_homo,
                  plot_object = g_std_curve_plot)
}

