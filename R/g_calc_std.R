##' @export

g_calc_std <- function(df_std,
                       std.conc.units = NULL,
                       std.spec.units = NULL,
                       #...) { # I *think* ... would work to put other objects into the list, but it isn't the best design decision. Better to just pass in an object that contains the bits you want
                       other_objects = NULL) {

  ### Will only accept a data frame
  assertthat::are_equal(class(df_std), "data.frame")

  assertable::assert_colnames(data = df_std,
                              colnames = c("std.conc",
                                           "spec.homo",
                                           "spec.buffer"),
                              only_colnames = FALSE,
                              quiet = TRUE)

  ### Code to check that other_objects is a list, unless it is NULL

  ### Create linear model for standard curve in homogenate
  lm_fit_homo <- lm(spec.homo ~ std.conc, data = df_std)

  ### Create linear model for standard curve in buffer
  lm_fit_buffer <- lm(spec.buffer ~ std.conc, data = df_std)


  ### Create vector of units
  units <- c("std.conc.units" = std.conc.units,
             "std.spec.units" = std.spec.units)

  ### Output list containing linear models and coefficients
  out_list <- list(g_std_lm_fit_homo = lm_fit_homo,
                   g_std_lm_fit_buffer = lm_fit_buffer,
                   g_std_data = df_std,
                   g_std_units = units)
  if(!is.null(other_objects)) {
    out_list <- c(out_list, other_objects)
  }

  ### Assign list to class of 'list' and subclass of 'ezmmek_g_std_curve'
  class(out_list) <- c("ezmmek_g_std", "list")

  out_list

}
