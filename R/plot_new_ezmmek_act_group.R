##' @export

plot.new_ezmmek_act_group <- function(df, ...) {

  ### User-defined columns to facet by
  columns <- rlang::enquos(...)

  ### German protocol
  if("act.raw.data.g" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act.raw.data = act.raw.data.g)

    unnest_act_df <- tidyr::unnest(df, act.raw.data)

    ### Make plot
    act_plot <- ggplot2::ggplot(data = unnest_act_df,
                                mapping = ggplot2::aes(x = sub.conc,
                                                       y = signal,
                                                       color = as.factor(replicate))) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_discrete(name = "replicate") +
      ggplot2::facet_wrap(columns)

    }

  ### Steen protocol
  if("act.raw.data.s" %in% colnames(df)) {
    df <- df %>% dplyr::rename(act.raw.data = act.raw.data.s)


    unnest_act_df <- tidyr::unnest(df, act.raw.data)

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
