get_padding <- function(input_shape, multiple_of) {
  pad1 <- -(input_shape[[1]] %% -multiple_of[1]) / 2
  pad2 <- -(input_shape[[2]] %% -multiple_of[2]) / 2
  pad3 <- -(input_shape[[3]] %% -multiple_of[3]) / 2

  list(
    list(floor(pad1), ceiling(pad1)),
    list(floor(pad2), ceiling(pad2)),
    list(floor(pad3), ceiling(pad3))
  )
}
