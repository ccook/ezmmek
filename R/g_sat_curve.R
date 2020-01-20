g_sat_curve <- function(d_std_g, d_sat_g, x.label = NULL, y.label = NULL, km = NULL, vmax = NULL) {

  ### Stop function if columns lack these specific names
  assertable::assert_colnames(data = d_std_g,
                              colnames = c("std.conc", "spec.homo", "spec.buffer", "replicate"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  assertable::assert_colnames(data = d_sat_g,
                              colnames = c("time",
                                           "replicate",
                                           "spec",
                                           "sub.conc",
                                           "assay.vol",
                                           "sub.control",
                                           "buffer.vol",
                                           "homo.vol",
                                           "soil.mass"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Add mean spec values of standard in buffer and standard in homogenate
  d_std_g_2 <- d_std_g %>%
    dplyr::group_by(std.conc) %>%
    dplyr::mutate(spec.m.homo = mean(spec.homo)) %>%
    dplyr::mutate(spec.m.buffer = mean(spec.buffer))

  ### Calculate slope of mean spec value of standard in homogenate
  lm_fit_homo <- lm(spec.m.homo ~ std.conc, data = d_std_g_2)
  lm.slope.homo <- coef(lm_fit_homo)[2]

  ### Calculate slope of mean spec value of standard in buffer
  lm_fit_buffer <- lm(spec.m.buffer ~ std.conc, data = d_std_g_2)
  lm.slope.buffer <- coef(lm_fit_buffer)[2]

  ### Add quench coef. and emission coef. to raw saturation data
  d_sat_g_2 <- d_sat_g %>%
    dplyr::group_by(sub.conc, time) %>%
    dplyr::mutate(quench.coef = lm.slope.homo / lm.slope.buffer) %>%
    dplyr::mutate(emission.coef = (lm.slope.homo / assay.vol))

  ### Create new dataframe that contains average spec values of homogenate controls over time
  d_sat_baseline <- d_sat_g_2 %>%
    filter(sub.conc == 0) %>%
    group_by(time) %>%
    summarize(spec.baseline.m = mean(spec),
              spec.baseline.sd = sd(spec))

  ### Create new dataframe that merges d_sat_g_2 with homogenate control values from d_sat_baseline
  d_sat_g_3 <- merge(d_sat_g_2, d_sat_baseline, by = ("time")) %>%
    dplyr::filter(time > 0) #Filter out Time 0, because cannot divide by 0 later

  ### Correct spec values by homogenate baseline control
  d_sat_g_3$spec.homogenate.corrected <- d_sat_g_3$spec - d_sat_g_3$spec.baseline.m

  ### Create new dataframe that calculate net fluorescence and activity
  d_sat_g_4 <- d_sat_g_3 %>%
    dplyr::mutate(net.fluor = (spec.homogenate.corrected / quench.coef) - sub.control) %>%
    dplyr::mutate(activity = (net.fluor * buffer.vol) / (emission.coef * homo.vol * time * soil.mass))

  ### Create new dataframe that contains mean and standard deviations of activity by substrate concentration
  d_sat_g_5 <- d_sat_g_4 %>%
    dplyr::group_by(sub.conc) %>%
    dplyr::mutate(activity.m = mean(activity), activity.sd = sd(activity)) %>%
    dplyr::filter(sub.conc > 0)


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


