check_ydims <- function(Y) {
  y.2d <- FALSE
  if (!is.null(dim(Y))) {
    d.y <- dim(Y)[2]
    if (d.y > 1) {
      y.2d <- TRUE
    }
  }
  return(y.2d)
}
