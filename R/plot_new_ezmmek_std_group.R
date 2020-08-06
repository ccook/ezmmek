##' plot_new_ezmmek_std_group
##'
##' @export
##'
##' @description Plots new_ezmmek_std_group object and facets by specified column names
##'
##' @param x data.frame object of class new_ezmmek_std_group
##' @param ... User defined column names by which to facet plot
##'
##' @examples
##' \dontrun{plot.new_ezmmek_std_group(new_ezmmek_std_group_obj,
##' site_name,
##' std_type)}

plot.new_ezmmek_std_group <- function(x, ...) {

  ### User-defined columns to facet by
  columns <- rlang::enquos(...)


  ### IBC protocol
  if("std_raw_data_ibc" %in% colnames(x)) {

    homo_plot <- ggplot2::ggplot(data = tidyr::unnest(x, std_raw_data_ibc),
                                 mapping = ggplot2::aes(x = std_conc, y = homo_signal)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(columns)

    buffer_plot <- ggplot2::ggplot(data = tidyr::unnest(x, std_raw_data_ibc),
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

  ### ISC protocol
  if("std_raw_data_isc" %in% colnames(df)) {

    ### Make plot
    homo_plot <- ggplot2::ggplot(data = tidyr::unnest(x, std_raw_data_isc),
                                 mapping = ggplot2::aes(x = std_conc, y = homo_buffer_signal)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(columns)

    homo_plot

  }

}

