pmlb.tasks <- c("classification", "regression")
pmlb.owner <- 'EpistasisLab'
pmlb.repo <- 'penn-ml-benchmarks'

#' Load from PMLB Dataset
#'
#' A function to load a specified dataset from the PMLB dataset.
#' @importFrom readr read_tsv
#' @param datasets the name of the dataset you wish to load. Defaults to \code{NULL}.
#' \itemize{
#' \item \code{NULL} Load all the datasets without specifying a specific name matching the desired query.
#' \item \code{'datasetid'} Returns the dataset with the desired id \code{'datasetid'} matching the desired query.
#' }
#' @param tasks the type of the tasks to return datasets for, either "classification" or "regression". Defaults to \code{NULL}.
#' \itemize{
#' \item \code{NULL} Return all datasets matching the desired query.
#' \item \code{'classification'} Load all classification datasets matching the desired query.
#' \item \code{'regression'} Load all regression datasets matching the desired query.
#' }
#' @param clean.nan whether to remove samples with invalid entries. Defaults to \code{TRUE}.
#' \itemize{
#' \item \code{TRUE} Remove samples that have features with \code{NaN} or non-finite.
#' \item \code{FALSE} Do not remove samples.
#' }
#' @param clean.ohe options for whether to one-hot-encode columns. Defaults to \code{FALSE}.
#' \itemize{
#' \item \code{clean.ohe < 1} Converts columns with < thr*n unique identifiers to one-hot encoded.
#' \item \code{is.integer(clean.ohe)} Converts columns with < thr unique identifiers to one-hot encoded.
#' \item \code{FALSE} Do not one-hot-encode any columns.
#' }
#' @return A list containing the following:
#' \itemize{
#' \item \code{data} The data for each dataset, as a list of the following:}
#' \itemize{
#' \item \code{X} \code{[n, d]} array with the \code{n} samples in \code{d} dimensions.
#' \item \code{Y} \code{[n]} vector with labels for each of the \code{n} samples. This item is only returned in the event that the dataset is a classification task.
#' }
#' \item \code{dsets.info} Useful metadata for each dataset. See \link{pmlb.list} for details.
#' }
#' @author Eric Bridgeford
#'
#' @examples
#' require(slbR)
#'
#' # load a specific dataset from pmlb
#' test <- pmlb.load(datasets="adult")
#' length(test$data) == 1  # the number of datasets requested
#' length(test$data$adult$Y) == 48842  # the number of known features in pmlb
#'
#'\dontrun{
#' test <- pmlb.load(tasks="classification")
#' length(test$data) == 166  # the number of classification tasks in pmlb
#'}
#'
#' # load 2 datasets from pmlb
#' test <- pmlb.load(datasets=c("adult", "chscase_geyser1"))
#' length(test$data) == 2
#' @export
pmlb.load <- function(datasets=NULL, tasks=NULL, clean.nan=TRUE, clean.ohe=FALSE) {
  pmlbpath <- 'https://github.com/EpistasisLab/penn-ml-benchmarks'

  dsets.query <- pmlb.list(datasets=datasets, tasks=tasks)
  data <- lapply(dsets.query$dsets.info, function(dset.info) {
    task <- dset.info$task
    if (task == 'regression') {
      dset.nm <- paste(dset.info$did, dset.info$name, sep="_")
    } else if (task == 'classification') {
      dset.nm <- dset.info$name
    }
    data <- as.matrix(suppressMessages(suppressWarnings(invisible(read_tsv(gzcon(url(file.path("https://raw.githubusercontent.com", pmlb.owner, pmlb.repo,
                                                                       "master/datasets", task, dset.nm, paste(dset.nm, ".tsv.gz", sep="")))),
                                                         col_names=TRUE)))))
    if (clean.nan || clean.ohe) {
      cleaned <- clean.dataset(data, clean.nan=clean.nan, clean.ohe=clean.ohe)
      samples <- cleaned$samples
      data <- cleaned$X
    } else {
      samples <- 1:dim(data)[1]
    }
    if (task == 'classification') {
      Y = as.matrix(data[,colnames(data) == 'target'])
      if (dim(Y)[2] == 1) {
        Y <- Y[,1]
      }
      return(list(X=as.matrix(data[,colnames(data) != 'target']), Y=Y, samples=samples))
    } else if (task == 'regression') {
      return(list(X=as.matrix(data), samples=samples))
    }
  })

  return(list(data=data, dsets.info=dsets.query))
}

pmlb.tasks <- c("classification", "regression")
pmlb.owner <- 'EpistasisLab'
pmlb.repo <- 'penn-ml-benchmarks'

#' List from PMLB Dataset
#'
#' A function to list available datasets for a specific task from the PMLB dataset. Also returns useful metadata.
#'
#' @importFrom httr GET
#' @importFrom readr read_tsv
#'
#' @param datasets the name of the dataset you wish to load. Defaults to \code{NULL}.
#' \itemize{
#' \item \code{NULL} Load all the datasets without specifying a specific name matching the desired query.
#' \item \code{'datasetid'} Returns the dataset with the desired id \code{'datasetid'} matching the desired query.
#' }
#' @param tasks the type of the task, either "classification" or "regression". Defaults to \code{NULL}.
#' \itemize{
#' \item \code{NULL} Return all datasets matching the desired query.
#' \item \code{'classification'} Load all classification datasets matching the desired query.
#' \item \code{'regression'} Load all regression datasets matching the desired query.
#' }
#' @return A list containing the following:
#' \itemize{
#' \item \code{dsets.meta} A \code{[length(tasks)]} element list, where each sublist contains a \code{tibble} where each row contains relevant metadata for the desired dataset.
#' \item \code{dsets.info} A \code{[n]} list where each element is named after a respective dataset, and each element contains the \code{did} and \code{task} of that dataset.
#' }
#' @author Eric Bridgeford
#'
#' @examples
#' require(slbR)
#'
#' # get information about all of the classification tasks in pmlb
#' test <- pmlb.list(tasks="classification")
#'
#' # get information about a specific dataset in pmlb
#' test <- pmlb.list(datasets="adult")
#'
#' # get information about 2 specific datasets in pmlb
#' test <- pmlb.list(datasets=c("adult", "chscase_geyser1"))
#' @export
pmlb.list <- function(datasets=NULL, tasks=NULL) {
  if (is.null(tasks)) {
    tasks <- pmlb.tasks
  }
  if (any(!(tasks %in% pmlb.tasks))) {
    stop("You have entered an invalue task type for the PMLB Repository.")
  }

  dsets.tasks <- lapply(tasks, function(task) {
    if (task == "regression") {
      task.id <- "Regression"
    } else {
      task.id <- task
    }
    dsets.task <- suppressMessages(suppressWarnings(invisible(read_tsv(file.path("https://raw.githubusercontent.com", pmlb.owner,
                                               pmlb.repo, "master/datasets", task, paste(task.id, "_datasets_pmlb.tsv", sep="")),
                                   col_names=TRUE))))
    if (!is.null(datasets)) {
      ss <- dsets.task$name %in% datasets
      if (sum(ss) == 0) {
        return(NULL)
      }
      dsets.task <- dsets.task[ss, ]
    }
    if (! "did" %in% names(dsets.task)) {
      did <- array(NA, dim=c(dim(dsets.task)[1]))
    } else {
      did <- dsets.task$did
    }
    return(list(dsets.task=dsets.task, names=dsets.task$name, did=did, tasks=array(task, dim=c(dim(dsets.task)[1]))))
  })

  if (length(dsets.tasks))

  names(dsets.tasks) <- tasks
  dsets.names <- do.call(c, lapply(dsets.tasks, function(dsets.task) dsets.task$names))
  if (length(dsets.names) == 0) {
    return(list(dsets.meta=NULL, dsets.info=NULL))
  }
  dsets.dids <- do.call(c, lapply(dsets.tasks, function(dsets.task) dsets.task$did))
  dsets.taskids <- do.call(c, lapply(dsets.tasks, function(dsets.task) dsets.task$tasks))
  names(dsets.dids) <- dsets.names; names(dsets.taskids) <- dsets.names

  dsets.info <- lapply(as.character(dsets.names), function(dset.name) list(task=as.character(dsets.taskids[dset.name]),
                                                                           did=as.character(dsets.dids[dset.name]),
                                                                           name=as.character(dset.name)))
  names(dsets.info) <- as.character(dsets.names)

  return(list(dsets.meta = dsets.tasks, dsets.info=dsets.info))
}
