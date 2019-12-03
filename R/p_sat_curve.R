##' Create saturation curve
##'
##' @description Creates a dataframe and plot by applying the standard curve coefficients to the raw saturation data.
##'
##' @param d_std  Must be a dataframe that contains 'std.conc' and 'spec'.
##' @param d_sat Must be a dataframe that contains 'time', 'sub.conc' (substrate concentration), 'replicate', and 'spec' (spectral data).
##' If d_sat contains a fifth column, that fifth column will be assumed to be a normalization factor.
##' The rate of reacation will be divided by the values present in the fifth column.
##' The user will be prompted to name the unit of normalization, which will appear on the y-axis, if a fifth column is present.
##' @return List containing new dataframe, regression model, and saturation curve
##' @details The spectral data is converted to concentration of standard.
##' The new dataframe contains the average slope (rate of reaction) and standard deviation for each replicate at each substrate concentration.
##' 'p_sat_curve' plots the new dataframe with substrate concentration on the x-axis, and rate of reaction on the y-axis.
##' It asks the user to specify axis labels with the appropriate units. It predicts and reports Vmax and Km values.
##' It also creates a list output containing the new dataframe, an additional new dataframe consisting of predicted curve fit values, the regression model, and the saturation curve plot.
##' @examples p_sat_curve(d_std, d_sat)
##' p_sat_curve(d_std, d_sat_n)
##' @author Christopher L. Cook and Andrew D. Steen
##' @export

########
# plot saturation curve and print km and vmax values
########

p_sat_curve <- function(d_std, d_sat) {


  ### stop function if d_std or d_sat columns lack these specific names
  if(!"std.conc" %in% names(d_std)) {
    stop("A column named 'std.conc' was expected but not provided")
  }

  if(!"spec" %in% names(d_std)) {
    stop("A column named 'spec' was expected but not provided")
  }

  if(!"time" %in% names(d_sat)) {
    stop("A column named 'time' was expected but not provided")
  }


  if(!"spec" %in% names(d_sat)) {
    stop("A column named 'spec' was expected but not provided")
  }


  if(!"replicate" %in% names(d_sat)) {
    stop("A column named 'replicate' was expected but not provided")
  }

  if(!"sub.conc" %in% names(d_sat)) {
    stop("A column named 'sub.conc' was expected but not provided")
  }

  ### convert fsu to conc. of standard and add to d_sat dataframe
  lm_fit <- lm(spec ~ std.conc, data = d_std)
  lm.intercept <- coef(lm_fit)[1]
  lm.slope <- coef(lm_fit)[2]
  d_sat$spec.to.std <- (d_sat$spec - lm.intercept)/lm.slope


  ### if original dataset contains five columns, assume fifth column to be a normalization factor
  ### factor in normalization factor to data
  if(ncol(d_sat) == 6) {

    ### create new column of reaction rate called 'activity.norm'
    d_sat <- dplyr::mutate(d_sat, activity.norm = spec.to.std/d_sat[,5])

    ### create new dataframe with average slope, standard deviation, and sub. conc. based on std curve coefs and activity.norm
    d_sat_2 <- d_sat %>% dplyr::group_by(sub.conc, replicate) %>%
      tidyr::nest() %>%
      dplyr::mutate(std.slope = purrr::map_dbl(data, function(df) coef(lm(activity.norm ~ time, data = df))[2])) %>%
      dplyr::group_by(sub.conc) %>%
      dplyr::mutate(slope.m = mean(std.slope), slope.sd = sd(std.slope))

    ### else normalization factor not accounted for
  }else{

    ### create new dataframe with average slope, standard deviation, and sub. conc. based on std curve coefs
    d_sat_2 <- d_sat %>% dplyr::group_by(sub.conc, replicate) %>%
      tidyr::nest() %>%
      dplyr::mutate(std.slope = purrr::map_dbl(data, function(df) coef(lm(spec.to.std ~ time, data = df))[2])) %>%
      dplyr::group_by(sub.conc) %>%
      dplyr::mutate(slope.m = mean(std.slope), slope.sd = sd(std.slope))
  }

  ### create vector of possible concentration units
  x.units.vec <- c("(M)","(mM)","(μM)", "(nM)")

  ### prompt user to name the type of substrate
  x.s <- readline(prompt = "Substrate name: ")

  ### ask user to choose which unit of concentration
  x.index.units <- menu(x.units.vec, graphics = FALSE, title = "x-axis: What are the units of substrate concentration?")

  ### assign value for x-axis label on plot
  plot.x.label <- paste(x.s, x.units.vec[x.index.units], sep = " ")

  ### assign value to superscript of '-1'
  sup.s <- "\U207B\U00B9"

  ### create vector of possible units of concentration
  y.units.vec.conc <- c("M","mM","μM","nM")

  ### create vector of possible units of time, with superscript
  y.units.vec.time <- c(paste("sec", sup.s, sep = ""),
                        paste("min", sup.s, sep = ""),
                        paste("hr", sup.s, sep = ""),
                        paste("day", sup.s, sep = ""))

  ### ask user to choose which unit of concentration
  y.index.units.conc <- menu(y.units.vec.conc, graphics = FALSE,
                             title = "y-axis: What are the units of concentration?")
  ### ask user to choose which unit of time
  y.index.units.time <- menu(y.units.vec.time, graphics = FALSE,
                             title = "y-axis: What are the units of time?")

  ### if data was normalized
  if("activity.norm" %in% names(d_sat)) {

    ### prompt user to name the unit of normalization
    norm.name <- readline(prompt = "Normalization unit: ")
    y.units.vec.norm <- c(paste(norm.name, sup.s, sep = ""))

    ### assign value for y-axis label on plot
    plot.y.label <- paste("Reaction Rate",
                          paste("(", y.units.vec.conc[y.index.units.conc], sep = ""),
                          y.units.vec.time[y.index.units.time],
                          paste(y.units.vec.norm, ")", sep = ""),
                          sep = " ")

    ### else there will be no prompt for normalization unit
  } else {

    ### assign value for y-axis label on plot
    plot.y.label <- paste("Reaction Rate",
                          paste("(",y.units.vec.conc[y.index.units.conc],sep = ""),
                          paste(y.units.vec.time[y.index.units.time],")",sep = ""),
                          sep = " ")
  }


  ### create plot with substrate conc. as x axis, and average slope as y axis
  p_sat_curve_1 <- ggplot2::ggplot(data = d_sat_2, mapping = ggplot2::aes(x = sub.conc, y = slope.m)) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::theme_bw() +
    ggplot2::xlab(plot.x.label) +
    ggplot2::ylab(plot.y.label) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 18)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = slope.m - slope.sd, ymax = slope.m + slope.sd, width = 15)) +
    ggplot2::scale_y_continuous(labels = scales::scientific)

  ### calculate saturation curve fit
  max.slope <- max(d_sat_2$slope.m)
  half.conc <- median(d_sat_2$sub.conc)

  mm_form <- formula(slope.m ~ (Vmax * sub.conc)/(Km + sub.conc))
  mm_fit <- nls2::nls2(formula = mm_form, data = d_sat_2,
                       start = list(Vmax = max.slope, Km = half.conc))

  ### print km and vmax variables to console
  print(summary(mm_fit))

  ### create a 1-column data frame with a 'grid' of points to predict
  min.sub.conc <- min(d_sat_2$sub.conc)
  max.sub.conc <- max(d_sat_2$sub.conc)
  pred_grid <- data.frame(sub.conc = min.sub.conc:max.sub.conc)

  ### put the predicted values into a data frame, paired with the values at which they were predicted
  predictions <- predict(mm_fit, newdata = pred_grid)

  pred_df <- data.frame(sub.conc = pred_grid$sub.conc, slope.m = predictions)

  ### create and print plot of saturation curve with fit
  p_sat_fit <- p_sat_curve_1 +
    ggplot2::geom_line(data = pred_df, ggplot2::aes(x = sub.conc, y = slope.m))

  plot(p_sat_fit)

  ### output list of predicted Vmax, predicted Km, d_sat_2 dataframe, mm_fit summary, and plot
  out_list <- list(sat_data = d_sat_2, curve_data = pred_df, fit_object = mm_fit, plot_object = p_sat_fit)

}


