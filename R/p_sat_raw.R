##' Create plot of raw data
##'
##' @description Creates plot of raw saturation data.
##'
##' @param d_sat Must be a dataframe that contains 'time', 'sub.conc' (substrate concentration), 'replicate', and 'spec' (spectral data).
##'
##' @return List containing plot.
##'
##' @details Plots raw saturation curve data into separate facets based on substrate concentration ('sub.conc').
##' It asks the user to specify the axis labels with the appropriate units.
##' It creates a list output containing the raw data plot.
##'
##' @examples #Run 'p_sat_raw(d_sat)'.
##' #When prompted 'x-axis: What are the units of time?', type '2' and press Enter.
##' #When prompted 'y-axis: Detection Unit?:', type 'FSU' and press Enter.
##'
##' @author Christopher L. Cook and Andrew D. Steen
##' @export

########
# plot raw data
########
p_sat_raw <- function(d_sat) {

  ### stop function if columns lack these specific names
  assertable::assert_colnames(data = d_sat,
                              colnames = c("time",
                                           "replicate",
                                           "spec",
                                           "sub.conc"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### create vector of different unit of concentration choices
  x.units.vec <- c("(sec)","(min)","(hr)", "(day)")

  ### ask user to choose which unit of concentration
  x.index.units <- menu(x.units.vec, graphics = FALSE, title =
                          "x-axis: What are the units of time?")

  ### assign value for x-axis label on plot
  plot.x.label <- paste("Time", x.units.vec[x.index.units], sep = " ")

  ### prompt user to name the unit of detection
  y.d <- readline(prompt = "y-axis: Detection unit?:")

  ### assign value for y-axis label on plot
  plot.y.label <- paste(y.d)

  ### create plot of raw saturation data
  p_sat_raw_1 <- ggplot2::ggplot(data = d_sat, mapping =
                                   ggplot2::aes(x = time, y = spec, color = as.factor(replicate))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::facet_wrap(~sub.conc, scales = "fixed") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 6)) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::xlab(plot.x.label) +
    ggplot2::ylab(plot.y.label) +
    ggplot2::scale_color_discrete(name = "Replicate") +
    ggplot2::theme_bw()
  print(p_sat_raw_1)

  ### output list of plot
  out_list <- list(plot_object = p_sat_raw_1)
}
