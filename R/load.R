available.repositories <- c("pmlb")

#' Load Datasets from Available Repositories
#'
#' A function to load a specified dataset from the PMLB dataset.
#' @param repository the name of the repository you would like to query a dataset from. Defaults to \code{NULL}.
#' \itemize{
#' \item{\code{NULL}}{Load all datasets matching the query from the particular repository.}
#' \item{\code{"pmlb"}}{Load datasets from the Penn Machine-Learning Benchmarks.}
#' \item{\code{c("repo1", "repo2", ...)}}{Load data from the indicated repositories.}
#' }
#' @param dataset the name of the dataset you wish to load. Defaults to \code{NULL}.
#' \itemize{
#' \item{\code{NULL}}{Load all the datasets without specifying a specific name matching the desired query.}
#' \item{\code{'datasetid'}}{Returns the dataset with the desired id matching the desired query.}
#' \item{\code{c("datasetid1", "datasetid2", ...)}}{Load data from the indicated datasets.}
#' }
#' @param task the type of the task, either "classification" or "regression". Defaults to \code{NULL}.
#' \itemize{
#' \item{\code{NULL}}{Return all datasets matching the desired query.}
#' \item{\code{'classification'}}{Load all classification datasets matching the desired query.}
#' \item{\code{'regression'}}{Load all regression datasets matching the desired query.}
#' \item{\code{c("taskid1", "taskid2", ...)}}{Load data for the indicated tasks.}
#' }
#' @return A list of lists, where each element is named for a dataset, containing the following:
#' \itemize{
#' \item{X}{\code{[n, d]} array with the \code{n} samples in \code{d} dimensions.}
#' \item{Y}{\code{[n]} vector with labels for each of the \code{n} samples. This item is only returned in the event that the dataset has a target.}
#' }
#'
#' @examples
#' require(slbR)
#' # request 1 specific dataset from the pmlb dataset
#' test <- load.datasets(repositories="pmlb", datasets="adult")
#' length(test$adult$Y) == 48842 # a known example from the pmlb dataset
#'
#' # request all of the pmlb classification datasets
#' test <- load.datasets(repositories="pmlb", tasks="classification")
#' length(test) <- 166  # validates that we loaded all of the classification datasets from pmlb
#' @author Eric Bridgeford
#' @export
load.datasets <- function(repositories=NULL, datasets=NULL, tasks=NULL) {
  if (is.null(repositories)) {
    repositories <- available.repositories
  }
  data.repos <- lapply(repositories, function(repository) {
    if (repository == 'pmlb') {
      data <- pmlb.load(datasets=datasets, tasks=tasks)$data
    }

    return(data)
  })

  data <- do.call(c, data.repos)

  return(data)
}
