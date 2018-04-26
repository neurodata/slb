
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
#' @return sets the cross-validation sets as an object of class \code{"XV"}. Each element of the list contains the following items:
#' \item{\code{X.train}}{the training data as a \code{[n - n/k, d]} array if \code{isTRUE(reverse)}, and a \code{[n/k, d]} array otherwise.}
#' \item{\code{Y.train}}{the training labels as a \code{[n - n/k]} vector or \code{[n - n/k, r]} array if \code{isTRUE(reverse)}, and a \code{[n/k, d]} vector or \code{[n/k, r]} array otherwise.}
#' \item{\code{X.test}}{the testing data as a \code{[n/k, d]} array if \code{isTRUE(reverse)}, and a \code{[n - n/k, d]} array otherwise.}
#' \item{\code{Y.test}}{the testing labels as a \code{[n/k]} vector or \code{[n/k, r]} array if \code{isTRUE(reverse)}, and a \code{[n -n/k]} vector or \code{[n - n/k, r]} array otherwise.}
#' @author Eric Bridgeford
#' @examples
#' # prepare data for 10-fold validation
#' library(slb)
#' data(SWD)
#' sets.xval.10fold <- lol.xval.split(SWD$X, SWD$Y, k=10)
#'
#' # prepare data for loo validation
#' sets.xval.loo <- lol.xval.split(SWD$X, SWD$Y, k='loo')
#'
#' # reverse the training and testing sets
#' sets.xval.10fold.rev <- lol.xval.split(SWD$X, SWD$Y, k=10, reverse=TRUE)
#' @export
slb.xval.split <- function(X, Y, k='loo', reverse=FALSE, ...) {
  n <- length(Y)
  n.x <- dim(X)[1]
  if (n != n.x) {
    stop("Your number of X samples and Y responses is not the same.")
  }
  if (k == 'loo') {
    k <- n  # loo is just xval with k=n
  }
  y.2d <- check_ydims(Y)
  if (round(k) == k) {  # then xval is an integer
    samp.ids <- as.matrix(sample(1:n, n))  # the sample ids randomly permuted
    k.folds <- split(samp.ids, rep(1:k), drop=TRUE)  # split the sample ids into xval folds
    # partition X and Y appropriately into training and testing sets
    sets <- lapply(k.folds, function(fold) {
      if (reverse) {
        train <- fold; test <- -fold
      } else {
        train <- -fold; test <- fold
      }
      if (y.2d) {
        Y.train <- Y[train,,drop=FALSE]
        Y.test <- Y[-train,,drop=FALSE]
      } else {
        Y.train <- Y[train,drop=FALSE]
        Y.test <- Y[-train,drop=FALSE]
      }
      list(X.train=X[train,,drop=FALSE], Y.train=Y.train,
           X.test=X[test,,drop=FALSE], Y.test=Y.train)
    })
  } else {
    stop("You have not entered a valid parameter for xval.")
  }
  return(sets)
}
