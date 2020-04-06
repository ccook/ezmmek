##' @export

plot.new_ezmmek_calibrate <- function(df, ..., columns = NULL) {

  ### User-defined columns to facet by if column names not supplied by parent fxn
  if(is.null(columns)) {
    columns <- rlang::enquos(...)
  }


  ### Correct for different column names with 'if' statements
  ### German protocol
  if("act.calibrated.data.g" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act.calibrated.data = act.calibrated.data.g,
                               std.raw.data = std.raw.data.g)
  }

  ### Steen protocol
  if("act.calibrated.data.s" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act.calibrated.data = act.calibrated.data.s,
                               std.raw.data = std.raw.data.s)
  }

  ### Unnest activity data
  unnest_cal_df <- tidyr::unnest(df, act.calibrated.data)

  ### Make plot of activity data
  cal_plot <- ggplot2::ggplot(data = unnest_cal_df,
                              mapping = ggplot2::aes(x = sub.conc,
                                                     y = activity.m)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = activity.m - activity.sd,
                                        ymax = activity.m + activity.sd)) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(columns)

cal_plot

}
