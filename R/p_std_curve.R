##' Create standard curve
##'
##' @description Creates a standard curve of spectral data versus fluorophore/chromophore concentration
##'
##' @param d_std  Must be a dataframe that contains 'std.conc' and 'spec'.
##'
##' @return List containing plot and fit model
##' @details Plots spectral data vs standard concentration.
##' It asks the user to specify the axis labels with the appropriate units.
##' It reports linear model summary statistics.
##' It also creates a list output containing the summary statistics and standard curve plot.
##' @examples p_std_curve(d_std)
##' @author Christopher L. Cook and Andrew D. Steen
##' @importFrom magrittr "%>%"
##' @export

########
# plot standard curve and print linear model stats
########
p_std_curve <- function(d_std) {

  ### stop function if columns lack these specific names
  if(!"std.conc" %in% names(d_std)) {
    stop("A column named 'std.conc' was expected but not provided")
  }

  if(!"spec" %in% names(d_std)) {
    stop("A column named 'spec' was expected but not provided")
  }

  ### create vector of different unit of concentration choices
  x.units.vec <- c("(M)","(mM)","(μM)", "(nM)")

  ### prompt user to name the type of standard used for curve
  x.s <- readline(prompt = "Standard type: ")

  ### ask user to choose which unit of concentration
  x.index.units <- menu(x.units.vec, graphics = FALSE, title = "x-axis: What are the units of concentration?")

  ### assign value for x-axis on plot
  plot.x.label <- paste(x.s, x.units.vec[x.index.units], sep = " ")

  ### prompt user to name unit of detection
  y.d <- readline(prompt = "y-axis: Detection unit?:")

  ### assign value for y-axis on plot
  plot.y.label <- paste(y.d)

  ### create plot of standard curve
  p_std_curve_1 <- ggplot2::ggplot(data = d_std, mapping = ggplot2::aes(x = std.conc, y = spec)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::theme_bw() +
    ggplot2::xlab(plot.x.label) +
    ggplot2::ylab(plot.y.label) +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 18)) +
    ggplot2::geom_smooth(method = "lm")
  print(p_std_curve_1)

  ### print linear stats
  lm_std_curve <- lm(formula = spec ~ std.conc, data = d_std)
  print(summary(lm_std_curve))

  ### output list of linear model and plot
  out_list <- list(fit_object = lm_std_curve, plot_object = p_std_curve_1)

}
