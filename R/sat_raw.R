##' Create plot of raw data
##'
##' @description Creates plot of raw saturation data.
##'
##' @param d_sat Must be a dataframe that contains 'time', 'sub.conc' (substrate concentration), 'replicate', and 'spec' (spectral data).
##' @param x.label The user must input a character string in the second argument to label the x-axis.
##' @param y.label The user must input a character string in the third argument to label the y-axis.
##'
##' @return List containing plot.
##'
##' @details Plots raw saturation curve data into separate facets based on substrate concentration ('sub.conc').
##' The user can label the x-axis by inputting a second argument.
##' The user can label the y-axis by inputting a third argument.
##' If the user does not define these arguments, the function will default to create a plot without labels.
##'
##' @examples
##' sat_raw(d_sat)
##' sat_raw(d_sat, "x-axis", "y-axis")
##'
##' @author Christopher L. Cook and Andrew D. Steen
##' @export

########
# Plot raw data
########
sat_raw <- function(d_sat, x.label = NULL, y.label = NULL) {

  ### Stop function if columns lack these specific names
  assertable::assert_colnames(data = d_sat,
                              colnames = c("time",
                                           "replicate",
                                           "spec",
                                           "sub.conc"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Assign values for x-axis and y-axis labels on plot
  plot.x.label <- x.label
  plot.y.label <- y.label

  ### Create plot of raw saturation data
  sat_raw_plot <- ggplot2::ggplot(data = d_sat, mapping =
                                      ggplot2::aes(x = time, y = spec, color =
                                                     as.factor(replicate))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::facet_wrap(~sub.conc, scales = "fixed") +
    ggplot2::theme(axis.text.y = ggplot2::element_text()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text()) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::xlab(plot.x.label) +
    ggplot2::ylab(plot.y.label) +
    ggplot2::scale_color_discrete(name = "Replicate") +
    ggplot2::theme_bw()
  print(sat_raw_plot)

  ### Output list of plot
  out_list <- list(plot_object = sat_raw_plot)
}
