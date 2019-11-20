########
# standard curve function for enzyme package

### plot standard curve and print linear model stats
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

  ### assign 'lm_stats' to access lm_std_curve summary and coefficients for later
  out_list <- list(fit_object = lm_std_curve, plot_object = p_std_curve_1)

}
