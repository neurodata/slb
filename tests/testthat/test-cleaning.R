context("Testing dataset cleaning...")

result.noclean <- slb.load.datasets(repositories="pmlb", datasets="adult", tasks="classification", clean.invalid=FALSE, clean.ohe=FALSE)
test_that("does not clean when not requested", {
  # run with no cleaning
  result.cleaned <- clean.dataset(result.noclean$adult, clean.invalid=FALSE, clean.ohe=FALSE)
  expect_equal(dim(result.cleaned$X), c(48842, 14))
})

test_that("one-hot-encodes as requested", {
  K <- 10
  result.cleaned <- clean.dataset(result.noclean$adult, clean.invalid=FALSE, clean.ohe = K)
  for (col in colnames(result.noclean$adult$X)) {
    num.unique <- length(unique(result.noclean$adult$X[, col]))
    if (num.unique < K & num.unique > 2) {
      expect_equal(num.unique, sum(colnames(result.cleaned$X) == col))
    } else {
      expect_equal(sum(colnames(result.cleaned$X) == col), 1)
    }
  }

  ohe <- 0.05
  K <- dim(result.noclean$adult$X)[1]*ohe
  result.cleaned <- clean.dataset(result.noclean$adult$X, clean.invalid=FALSE, clean.ohe = ohe)
  for (col in colnames(result.noclean$adult$X)) {
    num.unique <- length(unique(result.noclean$adult$X[, col]))
    if (num.unique < K & num.unique > 2) {
      expect_equal(num.unique, sum(colnames(result.cleaned$X) == col))
    } else {
      expect_equal(sum(colnames(result.cleaned$X) == col), 1)
    }
  }
})

test_that("purges NaNs as requested", {
  test <- apply(result.noclean$adult$X, c(1), sum)
  result.cleaned <- clean.dataset(result.noclean$adult$X, clean.invalid=TRUE, clean.ohe=FALSE)
  expect_equal(which(!is.nan(test)), result.cleaned$samp)
  expect_equal(length(result.cleaned$samp), dim(result.cleaned$X)[1])
})

context("Testing that drivers clean correctly...")

test_that("PMLB Driver", {
  result.cleaned <- load.datasets(repositories="pmlb", datasets="adult", tasks="classification", clean.invalid=TRUE, clean.ohe=FALSE)
  test <- apply(result.noclean$adult$X, c(1), sum)
  expect_equal(which(!is.nan(test)), result.cleaned$adult$samples)
  expect_equal(length(result.cleaned$adult$samples), dim(result.cleaned$adult$X)[1])
})
