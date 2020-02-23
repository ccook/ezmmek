
read_std_data <- function(std.data.fn, ...) {

  if(!(assertthat::are_equal(class(std.data.fn), "character"))) {
    stop("'std.data.fn' must be a CSV file as string quote")
  }

  std_data <- read.csv(std.data.fn)

  assertable::assert_colnames(data = std_data,
                              colnames = c("std.conc",
                                           "homo.spec",
                                           "std.type"),
                              only_colnames = FALSE,
                              quiet = TRUE)

    columns <- enquos(...)

    std_data_grouped <- std_data %>%
      group_by(!!! columns)

    out_list <- list(std_data_grouped = std_data_grouped)

    out_list

}




