context("Test hamming distance")

## test .convertIntoPairs

test_that("it properly converts", {
  vec <- 1:25; d <- 5
  res <- .convertIntoPairs(vec, d)
  
  expect_true(is.matrix(res))
  expect_true(all(dim(res) == c(2, 25)))
  expect_true(all(res >= 1))
  expect_true(all(res <= 5))
  expect_true(all(res[1,] == rep(1:5, each = 5)))
  expect_true(all(res[2,] == rep(1:5, times = 5)))
})

##################################

## test .generatePairs

test_that("it works normally", {
  numPairs <- 10
  dim3d <- rep(5,3)
  res <- .generatePairs(numPairs, dim3d)
  
  expect_true(is.numeric(res))
  expect_true(length(res) == 10)
  expect_true(length(unique(res)) == 10)
  expect_true(all(res >= 1))
  expect_true(all(res <= prod(dim3d)^2))
  expect_true(all(res %% 1 == 0))
})

#############################################

## test hammingDistance

test_that("hamming distance on itself should be 1", {
  set.seed(10)
  vec <- sample(0:4, 125, replace = T)
  parcellation <- brcbase::BrcParcellation(rep(5,3), vec)
  
  res <- hammingDistance(parcellation, parcellation)
  
  expect_true(res == 1)
})