##' @export

plot.new_ezmmek_calibrate <- function(df, ..., columns = NULL) {

  ### User-defined columns to facet by if column names not supplied by parent fxn
  if(is.null(columns)) {
    columns <- rlang::enquos(...)
  }


  ### Correct for different column names with 'if' statements
  ### German protocol
  if("act_calibrated_data_g" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act_calibrated_data = act_calibrated_data_g,
                               std_raw_data = std_raw_data_g)
  }

  ### Steen protocol
  if("act_calibrated_data_s" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act_calibrated_data = act_calibrated_data_s,
                               std_raw_data = std_raw_data_s)
  }

  ### Unnest activity data
  unnest_cal_df <- tidyr::unnest(df, act_calibrated_data)

  ### Make plot of activity data
  cal_plot <- ggplot2::ggplot(data = unnest_cal_df,
                              mapping = ggplot2::aes(x = substrate_conc,
                                                     y = activity_m)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = activity_m - activity_sd,
                                        ymax = activity_m + activity_sd)) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(columns)

cal_plot

}
