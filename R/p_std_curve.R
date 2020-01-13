##' Create standard curve
##'
##' @description Creates a standard curve of spectral data versus fluorophore/chromophore concentration.
##'
##' @param d_std  Must be a dataframe that contains 'std.conc' and 'spec'.
##' @param x.label The user must input a character string in the second argument to label the x-axis.
##' @param y.label The user must input a character string in the third argument to label the y-axis.
##'
##' @return List containing plot, fit model, and new dataframe.
##'
##' @details Plots spectral data vs standard concentration.
##' The user can label the x-axis by inputting a second argument.
##' The user can label the y-axis by inputting a third argument.
##' If the user does not define these arguments, the function will default to create a plot without labels.
##'
##' @examples
##' p_std_curve(d_std)
##' p_std_curve(d_std, "x-axis", "y-axis")
##'
##' @author Christopher L. Cook and Andrew D. Steen
##' @export

########
#Plot standard curve and print linear model stats
########
p_std_curve <- function(d_std, x.label = NULL, y.label = NULL) {

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
  p_std_curve_plot <- ggplot2::ggplot(data = d_std_2, mapping = ggplot2::aes(x = std.conc, y = spec.m)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::xlab(plot.x.label) +
    ggplot2::ylab(plot.y.label) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::theme(axis.text = ggplot2::element_text(), axis.title = ggplot2::element_text()) +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = spec.m - spec.sd, ymax = spec.m + spec.sd))

  ### Print linear stats
  lm_std_curve <- lm(formula = spec.m ~ std.conc, data = d_std_2)
  print(summary(lm_std_curve))

  ### Print plot
  print(p_std_curve_plot)

  ### Output list of linear model and plot
  out_list <- list(std_data = d_std_2, fit_object = lm_std_curve, plot_object = p_std_curve_plot)

}
