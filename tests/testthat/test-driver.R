context("Testing driver...")

test_that("load returns proper structuring under known contexts for PMLB", {
  result <- load.datasets(repositories="pmlb", datasets="adult", tasks="classification")
  expect_equal(length(result$adult$Y), 48842)
  expect_equal(length(result), 1)

  result <- load.datasets(repositories="pmlb", datasets="chscase_geyser1", tasks="regression")
  expect_equal(dim(result$chscase_geyser1$X)[1], 222)

  result <- load.datasets(repositories="pmlb", datasets=c("adult", "chscase_geyser1"))
  expect_equal(length(result), 2)
  expect_equal(length(result$adult$Y), 48842)
  expect_equal(dim(result$chscase_geyser1$X)[1], 222)

  result <- load.datasets(datasets="adult", tasks="classification")
  expect_equal(length(result$adult$Y), 48842)
  expect_equal(length(result), 1)
})
