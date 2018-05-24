# define this function
load.data <- function(...)
{
  e <- new.env()
  name <- data(..., envir = e)[1]
  e[[name]]
}
available.repositories <- c("pmlb", "uci", "mnist")

#' Load Datasets from Available Repositories
#'
#' A function to load a specified dataset from the PMLB dataset.
#' @param repositories the name of the repository you would like to query a dataset from. Defaults to \code{NULL}, which will return datasets matching from all repositories.
#' \itemize{
#' \item \code{NULL} Load datasets from all repositories matching other queries.
#' \item \code{"pmlb"} Load datasets from the Penn Machine-Learning Benchmarks.
#' \item \code{"uci"} Load datasets from the University of California - Irvine Machine Learning Repository.
#' \item \code{"mnist"} Load datasets from the MNIST dataset.
#' \item \code{c("repo1", "repo2", ...)} Load data from the indicated repositories.
#' }
#' @param datasets the name of the dataset you wish to load. Defaults to \code{NULL}.
#' \itemize{
#' \item \code{NULL} Load all the datasets without specifying a specific name matching the desired query.
#' \item \code{'datasetid'} Returns the dataset with the desired id matching the desired query.
#' \item \code{c("datasetid1", "datasetid2", ...)} Load data from the indicated datasets.
#' }
#' @param tasks the type of the task, either "classification" or "regression". Defaults to \code{NULL}.
#' \itemize{
#' \item \code{NULL} Return all datasets matching the desired query.
#' \item \code{'classification'} Load all classification datasets matching the desired query.
#' \item \code{'regression'} Load all regression datasets matching the desired query.
#' \item \code{c("taskid1", "taskid2", ...)} Load data for the indicated tasks.
#' }
#' @param clean.invalid whether to remove samples with invalid entries. Defaults to \code{TRUE}.
#' \itemize{
#' \item \code{TRUE} Remove samples that have features with \code{NaN} entries or non-finite.
#' \item \code{FALSE} Do not remove samples that have features with \code{NaN} entries or are non-finite..
#' }
#' @param clean.ohe options for whether to one-hot-encode columns. Defaults to \code{FALSE}.
#' \itemize{
#' \item \code{clean.ohe < 1} Converts columns with < thr*n unique identifiers to one-hot encoded.
#' \item \code{is.integer(clean.ohe)} Converts columns with < thr unique identifiers to one-hot encoded.
#' \item \code{FALSE} Do not one-hot-encode any columns.
#' }
#' @param verbose whether to print messages to the console if a repository or dataset is being ignored. defaults to \code{TRUE}.
#' @param ... trailing args.
#' @return A list of lists, where each element is a key-worded list for a particular benchmark dataset, containing at least the following:
#' \item{\code{X}}{\code{[n, d]} array with the \code{n} samples in \code{d} dimensions.}
#' \item{\code{Y}}{\code{[n]} vector or \code{[n, r]} array with responses for each of the \code{n} samples.}
#'
#' @examples
#' library(slb)
#' # request 1 specific dataset from the pmlb dataset
#' test <- slb.load.datasets(repositories="pmlb", datasets="ESL", clean.invalid=FALSE, clean.ohe=FALSE)
#' length(test$ESL$Y) == 488 # a known example from the pmlb dataset
#'
#' # request all of the pmlb classification datasets
#' \dontrun{
#' test <- slb.load.datasets(repositories="pmlb", tasks="classification")
#' length(test) <- 166  # validates that we loaded all of the classification datasets from pmlb
#' }
#' @author Eric Bridgeford
#' @export
slb.load.datasets <- function(repositories=NULL, datasets=NULL, tasks=NULL, clean.invalid=TRUE, clean.ohe=FALSE, verbose=TRUE, ...) {
  if (is.null(repositories)) {
    repositories <- available.repositories
  }
  data.repos <- lapply(repositories, function(repository) {
    # load the names of available datasets based on whether user wants classification or regression
    if (repository == "pmlb") {
      if (!is.null(tasks)) {
        if ("classification" %in% tasks) {
          dat.names <- as.character(load.data("pmlbClassTasks")[["dataset"]])
        } else if ("regression" %in% tasks) {
          dat.names <- as.character(load.data("pmlbRegTasks")[["dataset"]])
        }
      } else {
        dat.names <- c(as.character(load.data("pmlbClassTasks")[["dataset"]]),
                       as.character(load.data("pmlbRegTasks")[["dataset"]]))
      }
      # check if desired datasets are available in the given repository
      if (!is.null(datasets)) {
        # if datasets are specified, filter only for the ones the user queries for
        dat.names <- dat.names[dat.names %in% datasets]
      }
    } else if (repository == "uci") {
      if (!is.null(tasks)) {
        if ("classification" %in% tasks) {
          dat.names <- as.character(load.data("uciClassTasks")[["dataset"]])
        } else if ("regression" %in% tasks) {
          if (verbose) {
            print("uci repository does not contain Regression Tasks. Skipping...")
          }
        }
      } else {
        dat.names <- c(as.character(load.data("uciClassTasks")[["dataset"]]))
      }
      # check if desired datasets are available in the given repository
      if (!is.null(datasets)) {
        # if datasets are specified, filter only for the ones the user queries for
        dat.names <- dat.names[dat.names %in% datasets]
      }
    } else if (repository == "mnist") {
      if (!is.null(tasks)) {
        if ("classification" %in% tasks) {
          dat.names <- load.data("mnistClassTasks")[["dataset"]]
        } else if ("regression" %in% tasks) {
          if (verbose) {
            print("mnist repository does not contain Regression Tasks. Skipping...")
          }
        }
      } else {
        dat.names <- c(as.character(load.data("mnistClassTasks")[["dataset"]]))
      }
      # check if desired datasets are available in the given repository
      if (!is.null(datasets)) {
        # if datasets are specified, filter only for the ones the user queries for
        dat.names <- dat.names[dat.names %in% datasets]
      }
    } else {
      stop("You have attempted to pass an invalid repository. Supported repositories are c(\'pmlb\', \'uci\', \'mnist\').")
    }
    # load all specified datasets for the particular repository
    data <- lapply(dat.names, function(dat.name) {
      dat.name <- as.character(dat.name)
      x <- get(dat.name)
      if (clean.invalid || clean.ohe) {
        x <- clean.dataset(x, clean.invalid=clean.invalid, clean.ohe=clean.ohe)
      }
      return(x)
    })
    names(data) <- dat.names
    return(data)
  })
  return(do.call(c, data.repos))
}
