#' Dataset Cleansing
#'
#' A function for scrubbing a dataset for usage with most standard algorithms. This involves one-hot-encoding columns that are probably categorical.
#' @param data \code{[n, d]} array/matrix with the \code{n} samples in \code{d} dimensions.
#' @param clean.nan whether to remove samples with invalid entries. Defaults to \code{TRUE}.
#' \itemize{
#' \item{\code{TRUE}}{Remove samples that have features with \code{NaN} or non-finite.}
#' \item{\code{FALSE}}{Do not remove samples.}
#' }
#' @param clean.ohe options for whether to one-hot-encode columns. Defaults to \code{10}.
#' \itemize{
#' \item{\code{clean.ohe < 1}}{Converts columns with < thr*n unique identifiers to one-hot encoded.}
#' \item{\code{is.integer(clean.ohe)}}{Converts columns with < thr unique identifiers to one-hot encoded.}
#' \item{\code{FALSE}}{Do not one-hot-encode any columns.}
#' }
#' @return A list containing the following:
#' \itemize{
#' \item{X}{\code{[m, d+r]} the array with \code{m} samples in \code{d+r} dimensions, where \code{r} is the number of additional columns appended for encodings. \code{m < n} when  there are non-finite entries. \code{colnames(data)} returns the column names of the cleaned columns.}
#' \item{samples}{\code{m} the sample ids that are included in the final array, where \code{samp[i]} is the original row id corresponding to \code{Xc[i,]}. If \code{m < n}, there were non-finite entries that were purged.}
#' }
#' @author Eric Bridgeford
#' @export
clean.dataset <- function(data, clean.nan=TRUE, clean.ohe=10) {
  sumX <- apply(data, c(1), sum)
  samp <- which(!is.nan(sumX) & is.finite(sumX))
  X <- data[samp,]
  dimx <- dim(X)
  n <- dimx[1]; d <- dimx[2]
  if (clean.ohe < 1) {
    Kmax <- clean.ohe*n
  } else if (round(clean.ohe) == clean.ohe) {
    Kmax <- clean.ohe
  } else if (!isTRUE(clean.ohe)) {
    Kmax <- d
  }
  Xce <- lapply(1:d, function(i) {
    unx <- unique(X[,i])
    cname <- colnames(X)[i]
    x <- X[, i]
    if (cname != 'target') {
      K <- length(unx)
      if (K <= Kmax || K > 2) {
        # one-hot-encode
        x <- array(0, dim=c(n, K))
        for (j in 1:length(unx)) {
          x[which(X[,i] == unx[j]), j] <- 1
        }
      }
      enc <- array(cname, dim=c(ifelse(K > Kmax || K <= 2, 1, K)))
    } else {
      enc <- cname
    }
    return(list(enc=enc, x=x))
  })
  enc <- do.call(c, lapply(Xce, function(x) x$enc))
  Xc <- do.call(cbind, lapply(Xce, function(x) x$x))
  colnames(Xc) <- enc

  return(list(X=Xc, samples=samp))
}
