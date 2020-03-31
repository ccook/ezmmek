########
### Create new plot method for new_ezmmek_std_group
########

##' @export

plot.new_ezmmek_std_group <- function(new_ezmmek_std_group_obj) {

  if("std.raw.data.s" %in% colnames(new_ezmmek_std_group_obj)) {

  std_plot_homo <- ggplot2::ggplot(data = data.frame(new_ezmmek_std_group_obj$std.raw.data.s),
                              mapping = ggplot2::aes(x = std.conc, y = homo.signal)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::theme_bw()

  print(std_plot_homo)

  }

  if("std.raw.data.g" %in% colnames(new_ezmmek_std_group_obj)) {

    std_plot_homo <- ggplot2::ggplot(data = data.frame(new_ezmmek_std_group_obj$std.raw.data.g),
                                mapping = ggplot2::aes(x = std.conc, y = homo.signal)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::theme_bw()

    std_plot_buffer <- ggplot2::ggplot(data = data.frame(new_ezmmek_std_group_obj$std.raw.data.g),
                                     mapping = ggplot2::aes(x = std.conc, y = buffer.signal)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::theme_bw()

    print(std_plot_homo)
    print(std_plot_buffer)
  }

}

