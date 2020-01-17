##' Create saturation curve
##'
##' @description Creates a dataframe and plot by applying the standard curve coefficients to the raw saturation data.
##'
##' @param d_std  Must be a dataframe that contains 'std.conc' and 'spec'.
##' @param d_sat Must be a dataframe that contains 'time', 'sub.conc' (substrate concentration), 'replicate', and 'spec' (spectral data).
##' @param x.label The user must input a character string in the third argument to label the x-axis.
##' @param y.label The user must input a character string in the fourth argument to label the y-axis.

##'
##' @return List containing new dataframe, regression model, and saturation curve.
##'
##' @details The spectral data is converted to concentration of standard.
##' The new dataframe contains the average slope (rate of reaction) and standard deviation for each replicate at each substrate concentration.
##' 'sat_curve' plots the new dataframe with substrate concentration on the x-axis, and rate of reaction on the y-axis.
##' It predicts and reports Vmax and Km values.
##' It creates a list output containing the new dataframe, an additional new dataframe consisting of predicted curve fit values, the regression model, and the saturation curve plot.
##'
##' @examples
##' sat_curve(d_std, d_sat)
##' sat_curve(d_std, d_sat, "x-axis", "y-axis")
##'
##' @author Christopher L. Cook and Andrew D. Steen
##'
##' @importFrom magrittr "%>%"
##'
##' @export

########
# Plot saturation curve and print km and vmax values
########

sat_curve <- function(d_std, d_sat, x.label = NULL, y.label = NULL) {


  ### Stop function if d_std or d_sat columns lack these specific names
  assertable::assert_colnames(data = d_std,
                              colnames = c("std.conc", "spec"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  assertable::assert_colnames(data = d_sat,
                              colnames = c("time",
                                           "replicate",
                                           "spec",
                                           "sub.conc"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Create new dataframe with averages and standard deviations of replicates for standard curve
  d_std_2 <- d_std %>%
    dplyr::group_by(std.conc) %>%
    dplyr::mutate(spec.m = mean(spec), spec.sd = sd(spec))

  ### Convert fsu to conc. of standard and add to d_sat dataframe
  lm_fit <- lm(spec.m ~ std.conc, data = d_std_2)
  lm.intercept <- coef(lm_fit)[1]
  lm.slope <- coef(lm_fit)[2]
  d_sat$spec.to.std <- (d_sat$spec - lm.intercept)/lm.slope

  ### Create new dataframe with average slope, standard deviation, and sub. conc. based on std curve coefs
  d_sat_2 <- d_sat %>% dplyr::group_by(sub.conc, replicate) %>%
    tidyr::nest() %>%
    dplyr::mutate(std.slope = purrr::map_dbl(data, function(df) coef(lm(spec.to.std ~ time, data = df))[2])) %>%
    dplyr::group_by(sub.conc) %>%
    dplyr::mutate(slope.m = mean(std.slope), slope.sd = sd(std.slope))

### Assign values for x-axis and y-axis labels on plot
plot.x.label <- x.label
plot.y.label <- y.label

### Create plot with substrate conc. as x axis, and average slope as y axis
sat_curve_plot <- ggplot2::ggplot(data = d_sat_2, mapping = ggplot2::aes(x = sub.conc, y = slope.m)) +
  ggplot2::geom_point() +
  ggplot2::theme_bw() +
  ggplot2::xlab(plot.x.label) +
  ggplot2::ylab(plot.y.label) +
  ggplot2::theme(axis.text = ggplot2::element_text(),
                 axis.title = ggplot2::element_text()) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = slope.m - slope.sd, ymax = slope.m + slope.sd)) +
  ggplot2::scale_y_continuous(labels = scales::scientific)

### Calculate saturation curve fit
max.slope <- max(d_sat_2$slope.m)
half.conc <- median(d_sat_2$sub.conc)

mm_form <- formula(slope.m ~ (Vmax * sub.conc)/(Km + sub.conc))
mm_fit <- nls2::nls2(formula = mm_form, data = d_sat_2,
                     start = list(Vmax = max.slope, Km = half.conc))

### Print km and vmax variables to console
print(summary(mm_fit))

### Create a 1-column data frame with a 'grid' of points to predict
min.sub.conc <- min(d_sat_2$sub.conc)
max.sub.conc <- max(d_sat_2$sub.conc)
pred_grid <- data.frame(sub.conc = min.sub.conc:max.sub.conc)

### Put the predicted values into a data frame, paired with the values at which they were predicted
predictions <- predict(mm_fit, newdata = pred_grid)

pred_df <- data.frame(sub.conc = pred_grid$sub.conc, slope.m = predictions)

### Create and print plot of saturation curve with fit
sat_fit <- sat_curve_plot +
  ggplot2::geom_line(data = pred_df, ggplot2::aes(x = sub.conc, y = slope.m))

plot(sat_fit)

### Output list of predicted Vmax, predicted Km, d_sat_2 dataframe, mm_fit summary, and plot
out_list <- list(sat_data = d_sat_2, curve_data = pred_df, fit_object = mm_fit, plot_object = sat_fit)

}
