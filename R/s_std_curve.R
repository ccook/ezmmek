##' Create standard curve (bulk quench)
##'
##' @description Creates a standard curve of spectral data versus fluorophore/chromophore concentration.
##'
##' @param d_std  Must be a dataframe that contains 'std.conc', 'spec', and 'replicate'.
##' @param x.label Input a character string to label the x-axis.
##' @param y.label Input a character string to label the y-axis.
##'
##' @return List containing plot, fit model, and new dataframe.
##'
##' @details Plots spectral data vs standard concentration.
##' Can label the x-axis by inputting an argument.
##' Can label the y-axis by inputting an argument.
##' If these arguments are not defined, the function will default to create a plot without labels.
##'
##' @examples
##' s_std_curve(d_std)
##' s_std_curve(d_std, x.label = "x-axis label", y.label = "y-axis label")
##'
##' @author Christopher L. Cook and Andrew D. Steen
##' @export

########
#Plot standard curve and print linear model stats
########
s_std_curve <- function(d_std, x.label = NULL, y.label = NULL) {

  ### Stop function if columns lack these specific names
  assertable::assert_colnames(data = d_std,
                              colnames = c("std.conc", "spec", "replicate"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Create new dataframe with averages and standard deviations of replicates
  d_std_2 <- d_std %>%
    dplyr::group_by(std.conc) %>%
    dplyr::mutate(spec.m = mean(spec), spec.sd = sd(spec))

  ### Assign values for x-axis and y-axis labels on plot
  plot.x.label <- x.label
  plot.y.label <- y.label

  ### Create plot of standard curve
  std_curve_plot <- ggplot2::ggplot(data = d_std_2, mapping = ggplot2::aes(x = std.conc, y = spec)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::xlab(plot.x.label) +
    ggplot2::ylab(plot.y.label) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::theme(axis.text = ggplot2::element_text(), axis.title = ggplot2::element_text()) +
    ggplot2::geom_smooth(method = "lm")

  ### Print linear stats
  lm_std_curve <- lm(formula = spec.m ~ std.conc, data = d_std_2)
  print(summary(lm_std_curve))

  ### Print plot
  print(std_curve_plot)

  ### Output list of linear model and plot
  out_list <- list(std_data = d_std_2,
                   fit_object = lm_std_curve,
                   plot_object = std_curve_plot)

}
