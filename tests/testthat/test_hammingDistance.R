context("Test hamming distance")

##################################

## test .generatePairs

test_that("it works normally", {
  numPairs <- 10
  idx <- 1:25
  res <- .generatePairs(numPairs, idx)
  
  expect_true(is.matrix(res))
  expect_true(all(dim(res) == c(10,2)))
  expect_true(all(res >= 1))
  expect_true(all(res <= 25))
  expect_true(all(as.numeric(res) %% 1 == 0))
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