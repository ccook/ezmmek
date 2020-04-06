########
### Create new plot method for new_ezmmek_std_group
########

##' @export

plot.new_ezmmek_std_group <- function(df, ...) {

  ### User-defined columns to facet by
  columns <- rlang::enquos(...)


  ### German protocol
  if("std.raw.data.g" %in% colnames(df)) {

    homo_plot <- ggplot2::ggplot(data = tidyr::unnest(df, std.raw.data.g),
                                 mapping = ggplot2::aes(x = std.conc, y = homo.signal)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(columns)

    buffer_plot <- ggplot2::ggplot(data = tidyr::unnest(df, std.raw.data.g),
                                   mapping = ggplot2::aes(x = std.conc, y = buffer.signal)) +
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
  if("std.raw.data.s" %in% colnames(df)) {

    ### Make plot
    homo_plot <- ggplot2::ggplot(data = tidyr::unnest(df, std.raw.data.s),
                                 mapping = ggplot2::aes(x = std.conc, y = homo.signal)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(columns)

    homo_plot

  }

}

