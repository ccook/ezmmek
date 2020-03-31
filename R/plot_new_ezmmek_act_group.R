# plot.new_ezmmek_act_group <- function(new_ezmmek_act_group_obj) {
#
#   if("act.raw.data.s" %in% colnames(new_ezmmek_act_group_obj)) {
#
#     act_plot <- ggplot2::ggplot(data = new_ezmmek_act_group_obj,
#                                 mapping = ggplot2::aes(x = data.frame(new_ezmmek_act_group_obj$act.raw.data.s),
#                                                        y = data.frame(new_ezmmek_act_group_obj$act.raw.data.s),
#                                                        color = as.factor(data.frame(new_ezmmek_act_group_obj$act.raw.data.s))) +
#       ggplot2::geom_point() +
#       ggplot2::geom_smooth(method = "lm") +
#       ggplot2::theme_bw() +
#       ggplot2::facet_wrap(~sub.conc, scales = "fixed") +
#       ggplot2::scale_color_discrete(name = "replicate")
#
#     print(act_plot)
#   }
#
# }
