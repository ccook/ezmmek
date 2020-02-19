calc_std_fit <- function(df_std,
                         method) {

  if(method == "steen") {

    ### Stop function if columns in data frame lack these specific names
    assertable::assert_colnames(data = df_std,
                                colnames = c("std.conc",
                                             "homo.spec"),
                                only_colnames = FALSE,
                                quiet = TRUE)

    ### Fit linear model
    lm_fit <- lm(formula = homo.spec ~ std.conc, data = df_std)

    out_list <- list(lm_fit = lm_fit)
    }

  if(method == "german") {

    ### Will only accept a data frame
    assertthat::are_equal(class(df_std), "data.frame")

    assertable::assert_colnames(data = df_std,
                                colnames = c("std.conc",
                                             "homo.spec",
                                             "buffer.spec"),
                                only_colnames = FALSE,
                                quiet = TRUE)

    ### Code to check that other_objects is a list, unless it is NULL

    ### Create linear model for standard curve in homogenate
    lm_fit_homo <- lm(homo.spec ~ std.conc, data = df_std)

    ### Create linear model for standard curve in buffer
    lm_fit_buffer <- lm(buffer.spec ~ std.conc, data = df_std)

    out_list <- list(lm_fit_buffer = lm_fit_buffer,
                     lm_fit_homo = lm_fit_homo)
  }

  out_list
}
