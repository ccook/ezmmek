########
### Create new plot method for new_ezmmek_std_group
########

##' @export

plot.new_ezmmek_std_group <- function(df, ...) {

  ### User-defined columns to facet by
  columns <- rlang::enquos(...)


  ### German protocol
  if("std_raw_data_g" %in% colnames(df)) {

    homo_plot <- ggplot2::ggplot(data = tidyr::unnest(df, std_raw_data_g),
                                 mapping = ggplot2::aes(x = std_conc, y = homo_signal)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(columns)

    buffer_plot <- ggplot2::ggplot(data = tidyr::unnest(df, std_raw_data_g),
                                   mapping = ggplot2::aes(x = std_conc, y = buffer_signal)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(columns)

    print(homo_plot)
    print(buffer_plot)

    out_list <- list(std_homo_plot = homo_plot,
                     std_buffer_plot = buffer_plot)
    out_list
  }

  ### Steen protocol
  if("std_raw_data_s" %in% colnames(df)) {

    ### Make plot
    homo_plot <- ggplot2::ggplot(data = tidyr::unnest(df, std_raw_data_s),
                                 mapping = ggplot2::aes(x = std_conc, y = homo_buffer_signal)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(columns)

    homo_plot

  }

}

