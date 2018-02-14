context("testing metadata retrieval of datasets for PMLB...")

test_that("info stops or returns nothing when invalid dataset or task type specified", {
  expect_error(pmlb.list(datasets="adult", tasks="not a task"))
  expect_equal(pmlb.list(datasets="adult", tasks="regression"), list(dsets.meta=NULL, dsets.info=NULL))
})

test_that("info returns proper structuring under known contexts", {
  result <- pmlb.list(datasets="adult", tasks="classification")
  expect_equal(dim(result$dsets.meta$classification$dsets.task)[1], 1)
  expect_equal(result$dsets.meta$regression, NULL)
  expect_equal(length(result$dsets.info$adult$name), 1)

  result <- pmlb.list(datasets="chscase_geyser1", tasks="regression")
  expect_equal(dim(result$dsets.meta$regression$dsets.task)[1], 1)
  expect_equal(result$dsets.meta$classification, NULL)

  result <- pmlb.list(datasets=c("adult", "chscase_geyser1"))
  expect_equal(length(names(result$dsets.info)), 2)
})

context("testing loading of datasets for PMLB...")

test_that("load stops when invalid dataset or task type specified", {
  expect_error(pmlb.load(datasets="adult", tasks="not a task"))
  empty_res <- pmlb.load(datasets="adult", tasks="regression")
  expect_equal(empty_res$data, list())
})

test_that("load returns proper structuring under known contexts", {
  result <- pmlb.load(datasets="adult", tasks="classification")
  expect_equal(length(result$data$adult$Y), 48842)
  expect_equal(length(result$data), 1)

  result <- pmlb.load(datasets="chscase_geyser1", tasks="regression")
  expect_equal(dim(result$data$chscase_geyser1$X)[1], 222)

  result <- pmlb.load(datasets=c("adult", "chscase_geyser1"))
  expect_equal(length(result$data), 2)
  expect_equal(length(result$data$adult$Y), 48842)
  expect_equal(dim(result$data$chscase_geyser1$X)[1], 222)
})
