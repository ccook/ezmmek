##### Add lm of mpg ~ weight to mtcars
##### Add grid of points to each cell containing min:max range of values of mpg with
######### corresponding predicted values of wt based on lm

mtcars_obj <- function(df) {

  ### Add new column with same value in every cell
  df$new.col <- "same.name"

  ### Create new dataframe with lm object and predicted points
  mtcars_lm_col <- df %>%
    dplyr::group_by(new.col) %>%
    tidyr::nest() %>%
    dplyr::mutate(lm.col = purrr::map(data, function(df) mtcars_calc(df) %>%
                                        purrr::pluck(1)), #Pull lm
                  pred_df = purrr::map(data, function(df) mtcars_calc(df) %>%
                                         purrr::pluck(2))) %>% #Pull predicted df
    tidyr::unnest(data)

  mtcars_lm_col

}
