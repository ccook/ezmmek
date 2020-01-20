##' Create standard curves using German method
##'
##' @description Creates a standard curve of spectral data versus fluorophore/chromophore concentration.
##' Linear models and standard curves are generated for the standard in the presence of buffer and homogenate.
##' A third plot that overlays both standard curves is also generated.
##'
##' @param d_std_g  Must be a dataframe that contains 'std.conc', 'replicate', 'spec.homo', and 'spec.buffer'.
##' @param x.label The user must input a character string in the second argument to label the x-axis.
##' @param y.label The user must input a character string in the third argument to label the y-axis.
##'
##' @return List containing plots, fit models, and new dataframe.
##'
##' @details Plots spectral data vs standard concentration.
##' The user can label the x-axis by inputting a second argument.
##' The user can label the y-axis by inputting a third argument.
##' If the user does not define these arguments, the function will default to create a plot without labels.
##'
##' @examples
##' std_curve(d_std_g)
##' std_curve(d_std_g, "x-axis", "y-axis")
##'
##' @author Christopher L. Cook and Andrew D. Steen
##' @export

########
#Plot standard curve and print linear model stats
########
g_std_curve <- function(d_std_g, x.label = NULL, y.label = NULL) {


  ### Stop function if columns lack these specific names
  assertable::assert_colnames(data = d_std_g,
                              colnames = c("std.conc", "spec.homo", "spec.buffer", "replicate"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Add mean spec values of standard in buffer and standard in homogenate to new dataframe
  d_std_g_2 <- d_std_g %>%
    dplyr::group_by(std.conc) %>%
    dplyr::mutate(spec.m.homo = mean(spec.homo)) %>%
    dplyr::mutate(spec.m.buffer = mean(spec.buffer))

  ### Calculate slope of mean spec value of standard in homogenate
  lm_fit_homo <- lm(spec.m.homo ~ std.conc, data = d_std_g_2)
  print(summary(lm_fit_homo))

  ### Calculate slope of mean spec value of standard in buffer
  lm_fit_buffer <- lm(spec.m.buffer ~ std.conc, data = d_std_g_mean)
  print(summary(lm_fit_buffer))

  x.label <- x.label
  x.label <- y.label

  ### Generate plot of standard curve in homogenate
  g_std_curve_homo <- ggplot2::ggplot(data = d_std_g,
                                           mapping = aes(x = std.conc, y = spec.homo)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::xlab(x.label) +
    ggplot2::ylab(y.label) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::theme(axis.text = ggplot2::element_text(), axis.title = ggplot2::element_text()) +
    ggplot2::geom_smooth(method = "lm", color = "blue")

  print(g_std_curve_homo)

  ### Generate plot of standard curve in buffer
  g_std_curve_buffer <- ggplot2::ggplot(data = d_std_g,
                                           mapping = aes(x = std.conc, y = spec.buffer)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::xlab(x.label) +
    ggplot2::ylab(y.label) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::theme(axis.text = ggplot2::element_text(), axis.title = ggplot2::element_text()) +
    ggplot2::geom_smooth(method = "lm", color = "red")

  print(g_std_curve_buffer)

  ### Generate plot overlaying both standard curves
  g_std_curve_overlay <- ggplot2::ggplot() +
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

  print(g_std_curve_overlay)

  ### Output list containing new dataframe, linear models, and plots
  outlist <- list(std_data = d_std_g_2,
                  fit_buffer = summary(lm_fit_buffer),
                  fit_homogenate = summary(lm_fit_homo),
                  plot_object_overlay = g_std_curve_overlay,
                  plot_object_homogenate = g_std_curve_homo,
                  plot_object_buffer = g_std_curve_buffer)
}

