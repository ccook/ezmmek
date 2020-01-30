##' Create saturation curve (bulk quench)
##'
##' @description Creates a dataframe and plot by applying the standard curve coefficients to the raw saturation data.
##'
##' @param d_std_g Must be a dataframe that contains 'std.conc', 'replicate', 'spec.homo', and 'spec.buffer'.
##' @param d_sat_g Must be a dataframe that contains 'time', 'replicate', 'spec', 'sub.conc', 'assay.vol', 'sub.control', 'buffer.vol', 'homo.vol', and 'soil.mass'.
##' @param x.label Input a character string to label the x-axis. Default is 'NULL'.
##' @param y.label Input a character string to label the y-axis. Default is 'NULL'.
##' @param km Input a starting value to estimate 'km' value. Default value is median of 'sub.conc' values.
##' @param vmax Input a starting value to estimate 'vmax' value. Default value is the max activity calculated.

##'
##' @return List containing new dataframe, regression model, and saturation curve.
##'
##' @details The raw spectral data is used to calculate activity (unit concentration per unit mass per unit time)
##' The new dataframe contains the quench coefficient ('quench.coef), emission coefficient ('emission.coef'), homogenate corrected spectral values ('spec.homogenate.corrected'), and activity.
##' 'sat_curve' plots the new dataframe with substrate concentration on the x-axis, and rate of reaction on the y-axis.
##' It predicts and reports Vmax and Km values.
##' It creates a list output containing the new dataframe, an additional new dataframe consisting of predicted curve fit values, the regression model, and the saturation curve plot.
##'
##' @examples
##' s_sat_curve(d_std, d_sat)
##' s_sat_curve(d_std, d_sat, x.label = "x-axis label", y.label = "y-axis label", km = 97, vmax = 0.03)
##'
##' @author Christopher L. Cook and Andrew D. Steen
##'
##' @importFrom magrittr "%>%"
##'
##' @export

########
# Plot saturation curve and print km and vmax values
########

g_sat_curve <- function(d_std_g, d_sat_g, x.label = NULL, y.label = NULL, km = NULL, vmax = NULL) {

  ######
  # Add back in tests of argument values (assertable)
  ######



  ### Assign values for x-axis and y-axis labels on plot
  x.label <- x.label
  y.label <- y.label

  ### Create plot with substrate conc. as x-axis, and average activity as y-axis
  g_sat_curve_plot <- ggplot2::ggplot(data = d_sat_g_5,
                                      mapping = ggplot2::aes(x = sub.conc, y = activity.m)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::xlab(x.label) +
    ggplot2::ylab(y.label) +
    ggplot2::theme(axis.text = ggplot2::element_text(),
                   axis.title = ggplot2::element_text()) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = activity.m - activity.sd, ymax = activity.m + activity.sd)) +
    ggplot2::scale_y_continuous(labels = scales::scientific)

  ### Calculate saturation curve fit
  max.activity <- max(d_sat_g_5$activity.m)
  half.conc <- median(d_sat_g_5$sub.conc)

  if(is.null(km) | is.null(vmax)) {

    mm_form <- formula(activity.m ~ (vmax * sub.conc)/(km + sub.conc))
    mm_fit <- nls2::nls2(formula = mm_form, data = d_sat_g_5,
                         start = list(vmax = max.activity, km = half.conc))
  } else {

    km <- km
    vmax <- vmax

    mm_form <- formula(activity.m ~ (vmax * sub.conc)/(km + sub.conc))
    mm_fit <- nls2::nls2(formula = mm_form, data = d_sat_g_5,
                         start = list(vmax = vmax, km = km))
  }

  ### ## Print km and vmax variables to console
  print(summary(mm_fit))

  ### Create a 1-column data frame with a 'grid' of points to predict
  min.sub.conc <- min(d_sat_g_5$sub.conc)
  max.sub.conc <- max(d_sat_g_5$sub.conc)
  pred_grid <- data.frame(sub.conc = min.sub.conc:max.sub.conc)

  ### Put the predicted values into a data frame, paired with the values at which they were predicted
  predictions <- predict(mm_fit, newdata = pred_grid)

  pred_df <- data.frame(sub.conc = pred_grid$sub.conc, activity.m = predictions)

  ### Create and print plot of saturation curve with fit
  sat_fit <- g_sat_curve_plot +
    ggplot2::geom_line(data = pred_df, ggplot2::aes(x = sub.conc, y = activity.m))

  plot(sat_fit)

  ### Output list of predicted Vmax, predicted Km, d_sat_2 dataframe, mm_fit summary, and plot
  outlist <- list(sat_data = d_sat_g_5,
                  curve_data = pred_df,
                  fit_object = mm_fit,
                  plot_object = sat_fit)


}


