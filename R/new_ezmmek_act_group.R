#' new_ezmmek_act_group
#'
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @description Groups raw activity data by user-defined columns
#'
#' @param act.data.fn Activity data file as character string
#' @param ... User defined column names to join std.data.fn and act.data.fn
#' @param method Enzyme assay protocol. Must define method as 'steen' or 'german'
#' @param columns Column names carried over from parent functions if parent functions used
#'
#' @examples
#' new_ezmmek_act_group(act.data.fn, site.name, std.type, method = "steen", columns = NULL)

########
### Group raw activity data
########

new_ezmmek_act_group <- function(act.data.fn,
                                 ...,
                                 method = NA,
                                 columns = NULL) {

  ### Read in data
  act_data <- read.csv(act.data.fn)

  ### Use '...' arguments if column names not supplied in parent fxn
  if(is.null(columns)) {
    columns <- purrr::map_chr(rlang::enquos(...), rlang::quo_name)
  }

  ### Steen method required column names
  if(method == "steen") {
    assertable::assert_colnames(data = act_data,
                    colnames = c("time",
                                 "signal",
                                 "sub.conc"),
                    only_colnames = FALSE,
                    quiet = TRUE)


  act_data_grouped <- act_data %>%
    dplyr::group_by_at(dplyr::vars(intersect(names(.), columns))) %>%
    dplyr::group_nest(.key = "act.raw.data.s")
  }

  if(method == "german") {
    assertable::assert_colnames(data = act_data,
                    colnames = c("time",
                                 "signal",
                                 "sub.conc",
                                 "buffer.vol",
                                 "homo.vol",
                                 "soil.mass",
                                 "assay.vol",
                                 "homo.control",
                                 "sub.control"),
                    only_colnames = FALSE,
                    quiet = TRUE)

    act_data_grouped <- act_data %>%
      dplyr::group_by_at(dplyr::vars(intersect(names(.), columns))) %>%
      dplyr::group_nest(.key = "act.raw.data.g")
  }

  class(act_data_grouped) <- c("new_ezmmek_act_group", "data.frame")

  act_data_grouped

}
