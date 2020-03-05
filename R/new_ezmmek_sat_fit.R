new_ezmmek_sat_fit <- function(std.data.fn,
                               act.data.fn,
                               ...,
                               km = NULL,
                               vmax = NULL,
                               method = NA) {

  ### User names columns to be grouped
  columns <- map_chr(enquos(...), quo_name)

  calibrated_df <- new_ezmmek_act_calibrate(std.data.fn,
                                            act.data.fn,
                                            ...,
                                            method = method,
                                            conc.units = NA,
                                            signal.units = NA,
                                            columns = columns)

  calibrated_df_grouped <- calibrated_df %>%
    group_by_at(vars(sub.type, intersect(names(.), columns))) %>%
    nest()


  calibrated_df_mm_fit <- calibrated_df_grouped %>%
    mutate(mm.fit.obj = map(data, function(df) calculate_mm_fit(df, km, vmax)),
           km = map_dbl(data, function(df) coef(calculate_mm_fit(df, km, vmax))[2]),
           vmax = map_dbl(data, function(df) coef(calculate_mm_fit(df, km, vmax))[1])) %>%
    unnest(data)

  calibrated_df_mm_fit
}
