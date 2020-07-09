#' new_ezmmek_std_group
#'
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @description Groups standard curve data by user-defined columns
#'
#' @param std.data.fn Standard data file as character string
#' @param ... User defined column names to group std.data.fn
#' @param method Enzyme assay protocol. Must define method as 'isc' or 'ibc'
#' @param columns Column names carried over from parent functions if parent functions used
#'
#' @examples
#' new_obj <- new_ezmmek_std_group("data/tyson_std_04172020.csv",
#'   site.name,
#'   std.type,
#'   method = "isc",
#'   columns = NULL)
#' new_obj <- new_ezmmek_std_group("data/tyson_std_04172020.csv",
#'   site.name,
#'   std.type,
#'   method = "ibc",
#'   columns = NULL)

########
### Group standard lm objects
########

new_ezmmek_std_group <- function(std.data.fn,
                                 ...,
                                 method = NA,
                                 columns = NULL) {

  ### Read in data
  std_data <- read.csv(std.data.fn)

  ### Use '...' arguments if column names not supplied in parent fxn
  if(is.null(columns)) {
    columns <- purrr::map_chr(rlang::enquos(...), rlang::quo_name)
  }

  ### Group standard data
  std_data_grouped <- ezmmek_std_lm(std_data,
                                    columns = columns,
                                    method = method)

  ### Assign new class
  class(std_data_grouped) <- c("new_ezmmek_std_group", "data.frame")

  std_data_grouped

}

