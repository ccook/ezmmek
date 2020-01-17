g_sat_curve <- function(d_std_g, d_sat_g, x.label = NULL, y.label = NULL) {

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
  d_std_g_mean <- d_std_g %>%
    dplyr::group_by(std.conc) %>%
    dplyr::mutate(spec.m.homo = mean(spec.homo)) %>%
    dplyr::mutate(spec.m.buffer = mean(spec.buffer))

  ### Calculate slope of mean spec value of standard in homogenate
  lm_fit_homo <- lm(spec.m.homo ~ std.conc, data = d_std_g_mean)
  lm.slope.homo <- coef(lm_fit_homo)[2]

  ### Calculate slope of mean spec value of standard in buffer
  lm_fit_buffer <- lm(spec.m.buffer ~ std.conc, data = d_std_g_mean)
  lm.slope.buffer <- coef(lm_fit_buffer)[2]

  ### Add quench coef. and emission coef. to raw saturation data
  d_sat_g_coef <- d_sat_g %>%
    dplyr::group_by(sub.conc) %>%
    dplyr::mutate(quench.coef = lm.slope.homo / lm.slope.buffer) %>%
    dplyr::mutate(emission.coef = (lm.slope.homo / assay.vol))

  ### Create new dataframe that calculates mean homogenate control spec values over time
  d_sat_baseline <- d_sat_g_coef %>%
    filter(sub.conc == 0) %>%
    group_by(time) %>%
    summarize(spec.baseline.m = mean(spec),
              spec.baseline.sd = sd(spec))

  d_sat_g_merge <- merge(d_sat_g_coef, d_sat_baseline, by = ("time"))

  d_sat_g_merge$spec.homogenate.corrected <- d_sat_g_merge$spec - d_sat_g_merge$spec.baseline.m

  d_sat_g_merge$time[d_sat_g_merge$time == 0]<-0.001

  d_sat_g_act <- d_sat_g_merge %>%
    dplyr::mutate(net.fluor = (spec.homogenate.corrected / quench.coef) - sub.control) %>%
    dplyr::mutate(activity = (net.fluor * buffer.vol) / (emission.coef * homo.vol * time * soil.mass))

  d_sat_g_act_avg <- d_sat_g_act %>%
    dplyr::group_by(sub.conc) %>%
    dplyr::mutate(activity.m = mean(activity), activity.sd = sd(activity))


  ### Assign values for x-axis and y-axis labels on plot
  x.label <- x.label
  y.label <- y.label

  g_sat_curve_plot <- ggplot2::ggplot(data = d_sat_g_act_avg,
                                      mapping = ggplot2::aes(x = sub.conc, y = activity.m)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::xlab(x.label) +
    ggplot2::ylab(y.label) +
    ggplot2::theme(axis.text = ggplot2::element_text(),
                   axis.title = ggplot2::element_text()) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = activity.m - activity.sd, ymax = activity.m + activity.sd))
    ggplot2::scale_y_continuous(labels = scales::scientific)

  max.activity <- max(d_sat_g_act_avg$activity.m)
  half.conc <- median(d_sat_g_act_avg$sub.conc)

  mm_form <- formula(activity.m ~ (Vmax * sub.conc)/(Km + sub.conc))
  mm_fit <- nls2::nls2(formula = mm_form, data = d_sat_g_act_avg,
                       start = list(Vmax = max.activity, Km = half.conc))

  print(summary(mm_fit))

  min.sub.conc <- min(d_sat_g_act_avg$sub.conc)
  max.sub.conc <- max(d_sat_g_act_avg$sub.conc)
  pred_grid <- data.frame(sub.conc = min.sub.conc:max.sub.conc)

  predictions <- predict(mm_fit, newdata = pred_grid)

  pred_df <- data.frame(sub.conc = pred_grid$sub.conc, activity.m = predictions)

  sat_fit <- g_sat_curve_plot +
    ggplot2::geom_line(data = pred_df, ggplot2::aes(x = sub.conc, y = activity.m))

  plot(sat_fit)

  outlist <- list(sat_data = d_sat_g_act_avg,
                  #fit_buffer = lm_fit_buffer,
                  plot_object = g_sat_curve_plot)


}


