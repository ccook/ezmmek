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
#' @param method Enzyme assay protocol. Must define method as 'isc' or 'ibc'
#' @param columns Column names carried over from parent functions if parent functions used
#'
#' @examples
#' \dontrun{new_obj <- new_ezmmek_act_group("data/tyson_sat_steen_04172020.csv,
#'   site_name,
#'   std_type,
#'   method = "isc",
#'   columns = NULL)
#' new_obj <- new_ezmmek_act_group("data/tyson_sat_german_04172020.csv,
#'   site_name,
#'   std_type,
#'   method = "ibc",
#'   columns = NULL)}

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

  ### ISC method required column names
  if(method == "isc") {
    assertable::assert_colnames(data = act_data,
                    colnames = c("time",
                                 "signal",
                                 "substrate_conc"),
                    only_colnames = FALSE,
                    quiet = TRUE)


  act_data_grouped <- act_data %>%
    dplyr::group_by_at(dplyr::vars(intersect(names(.), columns))) %>%
    dplyr::group_nest(.key = "act_raw_data_isc")
  }

  ### IBC method required column names
  if(method == "ibc") {
    assertable::assert_colnames(data = act_data,
                    colnames = c("time",
                                 "signal",
                                 "substrate_conc",
                                 "buffer_vol",
                                 "homo_vol",
                                 "soil_mass",
                                 "std_vol",
                                 "homo_control",
                                 "substrate_control"),
                    only_colnames = FALSE,
                    quiet = TRUE)

    act_data_grouped <- act_data %>%
      dplyr::group_by_at(dplyr::vars(intersect(names(.), columns))) %>%
      dplyr::group_nest(.key = "act_raw_data_ibc")
  }

  class(act_data_grouped) <- c("new_ezmmek_act_group", "data.frame")

  act_data_grouped

}
