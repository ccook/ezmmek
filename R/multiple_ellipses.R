### Primary function has ellipses argument, but want secondary functions to each have a unique ellipses argument
primary_function <- (x, y, z, ...) {

  my_dots <- enqous(...)

  secondary_function_1 <- (x, y, my_dots)

  secondary_function_2 <- (z, my_dots)

}

### Tried putting in two ellipses argument into primary function, does not work (unsurprisingly)
primary_function <- (x, y, z, ..., ...) {

my_dots_1 <- enquos(...)
my_dots_2 <- enqous(...)

secondary_function_1 <- (x, y, my_dots_1)

secondary_function_2 <- (z, my_dots_2)

}
