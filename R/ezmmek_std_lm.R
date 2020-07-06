#' ezmmek_std_lm
#'
#' @importFrom magrittr "%>%"
#'
#' @description Creates dataframe of standard curve models as determined by grouping of user-defined columns
#'
#' @param df Standard curve dataframe
#' @param method Enzyme assay protocol. Must define method as 'steen' or 'german'
#' @param columns Column names carried over from parent functions if parent functions used

########
### Calculate standard curve linear models
########
ezmmek_std_lm <- function(df,
                          method = method,
                          columns = NULL) {

  ### Stop function if method is not assigned approriately
  if(
    !(method == "isc") & !(method == "ibc")
  ) {
    stop("method must equal 'isc' or 'ibc'")
  }

  if("std_conc" %in% columns) {
    stop("Cannot group arguments used to calculate linear model ('std_conc', 'homo_signal', 'buffer_signal')")
  }

  ### Steen method
  if(method == "isc") {

    ### Require certain column names
    assertable::assert_colnames(data = df,
                                colnames = c("std_conc",
                                             "homo_buffer_signal"),
                                only_colnames = FALSE,
                                quiet = TRUE)

    ###### Groups data by user-decided column names
    ######### Creates dataframe containing lm list for each unique set of grouped column
    std_data_lm <- df %>%
      dplyr::group_by_at(dplyr::vars(intersect(names(.), columns))) %>%
      dplyr::group_nest(.key = "std_raw_data_isc") %>%
      dplyr::mutate(std_lm_homo_buffer_obj = purrr::map(std_raw_data_isc, function(df) ezmmek_calc_std_lm_homo_buffer(df)), #homogenate lm
                    std_lm_homo_buffer_slope = purrr::map_dbl(std_raw_data_isc, function(df) coef(ezmmek_calc_std_lm_homo_buffer(df))[2]), #homogenate slope
                    std_lm_homo_buffer_intercept = purrr::map_dbl(std_raw_data_isc, function(df) coef(ezmmek_calc_std_lm_homo_buffer(df))[1]) #homogenate intercept
      )
  }

  ### German method
  if(method == "ibc") {

    ### Require certain column names
    assertable::assert_colnames(data = df,
                                colnames = c("std_conc",
                                             "homo_signal",
                                             "buffer_signal"),
                                only_colnames = FALSE,
                                quiet = TRUE)

    ###### Groups data by user-decided column names
    ######### Creates dataframe containing lm list for each unique set of grouped column
    std_data_lm <- df %>%
      dplyr::group_by_at(dplyr::vars(intersect(names(.), columns))) %>%
      dplyr::group_nest(.key = "std_raw_data_ibc") %>%
      dplyr::mutate(std_lm_homo_obj = purrr::map(std_raw_data_ibc, function(df) ezmmek_calc_std_lm_homo(df)), #homogenate lm
                    std_lm_homo_slope = purrr::map_dbl(std_raw_data_ibc, function(df) coef(ezmmek_calc_std_lm_homo(df))[2]), #homogenate slope
                    std_lm_homo_intercept = purrr::map_dbl(std_raw_data_ibc, function(df) coef(ezmmek_calc_std_lm_homo(df))[1]), #homogenate intercept
                    std_lm_buffer_obj = purrr::map(std_raw_data_ibc, function(df) ezmmek_calc_std_lm_buffer(df)), #buffer lm
                    std_lm_buffer_slope = purrr::map_dbl(std_raw_data_ibc, function(df) coef(ezmmek_calc_std_lm_buffer(df))[2]), #buffer slope
                    std_lm_buffer_intercept = purrr::map_dbl(std_raw_data_ibc, function(df) coef(ezmmek_calc_std_lm_buffer(df))[1]), #buffer intercept
                    quench_coef = std_lm_homo_slope / std_lm_buffer_slope #quench coefficient
      )
  }

  std_data_lm

}
