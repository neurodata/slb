#' Cross-Validation Data Splitter
#'
#' A function to split a dataset into
#' training and testing sets for cross validation.
#'
#' @param X \code{[n, d]} the data with \code{n} samples in \code{d} dimensions.
#' @param Y \code{[n]} vector or \code{[n, r]} array the responses for the samples.
#' @param k the cross-validated method to perform. Defaults to \code{'loo'}.
#' \itemize{
#' \item{if \code{k == round(k)}, performed k-fold cross-validation.}
#' \item{if \code{k == 'loo'}, performs leave-one-out cross-validation.}
#' }
#' @param reverse whether to flip the training and testing partitions. Defaults to \code{FALSE}.
#' \itemize{
#' \item{if \code{isTRUE(reverse)}, flips the training and testing partitions, such that cross-validation will be performed with 1 training fold and k-1 testing folds.}
#' \item{if \code{!isTRUE(reverse)}, standard cross-validation, with k-1 training folds and 1 testing fold.}
#' }
#' @param ... optional args.
#' @return sets the cross-validation sets as an object of class \code{"XV"} containing the following:
#' \item{\code{train}}{is \code{[n - n/k]} vector if \code{isTRUE(reverse)}, and a \code{[n/k]} vector otherwise.}
#' \item{\code{test}}{is \code{[n/k]} vector if \code{isTRUE(reverse)}, and a \code{[n - n/k]} vector otherwise.}
#' @author Eric Bridgeford
#' @examples
#' # prepare data for 10-fold validation
#' library(slb)
#' data(SWD)
#' sets.xval.10fold <- slb.xval.split(SWD$X, SWD$Y, k=10)
#'
#' # prepare data for loo validation
#' sets.xval.loo <- slb.xval.split(SWD$X, SWD$Y, k='loo')
#'
#' # reverse the training and testing sets
#' sets.xval.10fold.rev <- slb.xval.split(SWD$X, SWD$Y, k=10, reverse=TRUE)
#' @export
slb.xval.split <- function(X, Y, k='loo', reverse=FALSE, ...) {
  y.2d <- check_ydims(Y)
  if (y.2d) {
    n <- dim(Y)[1]
  } else {
    n <- length(Y)
  }
  n.x <- dim(X)[1]
  if (n != n.x) {
    stop("Your number of X samples and Y responses is not the same.")
  }
  if (k == 'loo') {
    k <- n  # loo is just xval with k=n
  }
  if (round(k) == k) {  # then xval is an integer
    samp.ids <- as.matrix(sample(1:n, n))  # the sample ids randomly permuted
    k.folds <- split(samp.ids, rep(1:k), drop=TRUE)  # split the sample ids into xval folds
    # partition X and Y appropriately into training and testing sets
    sets <- lapply(k.folds, function(fold) {
      if (reverse) {
        train <- samp.ids[fold]; test <- samp.ids[-fold]
      } else {
        train <- samp.ids[-fold]; test <- samp.ids[fold]
      }
      list(train=train, test=test)
    })
  } else {
    stop("You have not entered a valid parameter for k.")
  }
  return(structure(sets, class="XV"))
}
