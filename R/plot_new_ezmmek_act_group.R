##' @export

plot.new_ezmmek_act_group <- function(df, ...) {

  ### User-defined columns to facet by
  columns <- rlang::enquos(...)

  ### Use 'if' statements to adjust column names
  ### German protocol
  if("act_raw_data_ibc" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act_raw_data = act_raw_data_ibc)

    unnest_act_df <- tidyr::unnest(df, act_raw_data)

    ### Make plot
    act_plot <- ggplot2::ggplot(data = unnest_act_df,
                                mapping = ggplot2::aes(x = substrate_conc,
                                                       y = signal,
                                                       color = as.factor(replicate))) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_discrete(name = "replicate") +
      ggplot2::facet_wrap(columns)

    }

  ### Steen protocol
  if("act_raw_data_isc" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act_raw_data = act_raw_data_isc)


    unnest_act_df <- tidyr::unnest(df, act_raw_data)

    ### Make plot
    act_plot <- ggplot2::ggplot(data = unnest_act_df,
                                mapping = ggplot2::aes(x = time,
                                                       y = signal,
                                                       color = as.factor(replicate))) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::theme_bw() +
      ggplot2::scale_color_discrete(name = "replicate") +
      ggplot2::facet_wrap(columns)

    }

    act_plot

}
