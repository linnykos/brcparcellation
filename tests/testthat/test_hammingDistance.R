context("Test hamming distance")

## test .generatePairs

test_that("it works normally", {
  numPairs <- 10
  idx <- 1:25
  res <- .generatePairs(numPairs, idx)
  
  expect_true(is.matrix(res))
  expect_true(all(dim(res) <= c(10,2)))
  expect_true(all(res >= 1))
  expect_true(all(res <= 25))
  expect_true(all(as.numeric(res) %% 1 == 0))
})

#############################################

## test hammingDistance

test_that("hamming distance on itself is 1", {
  set.seed(10)
  vec <- sample(0:4, 125, replace = T)
  parcellation <- brcbase::BrcParcellation(rep(5,3), vec)
  
  res <- hammingDistance(parcellation, parcellation)
  
  expect_true(res == 1)
})

test_that("hamming distance for a parcellation and subparcellation is high", {
  set.seed(10)
  vec <- sample(0:5, 125, replace = T)
  vec2 <- vec
  idx <- which(vec2 == 0)
  vec2[idx] <- sample(c(0,6), length(idx), replace = T)
  
  parcellation1 <- brcbase::BrcParcellation(rep(5,3), vec)
  parcellation2 <- brcbase::BrcParcellation(rep(5,3), vec2)
  
  res <- hammingDistance(parcellation1, parcellation2)
  
  expect_true(res > 0.5)
})

test_that("hamming distance between 2 parcellations labeled diff is 1", {
  set.seed(10)
  vec <- sample(0:4, 125, replace = T)
  parcellation1 <- brcbase::BrcParcellation(rep(5,3), vec)
  
  vec2 <- plyr::mapvalues(vec, from = 0:4, c(1,0,3,4,2))
  parcellation2 <- brcbase::BrcParcellation(rep(5,3), vec2)
  
  res <- hammingDistance(parcellation1, parcellation2)
  
  expect_true(res == 1)
})

test_that("hamming dist between single parcellation and the singleton is 0", {
  parcellation1 <- brcbase::BrcParcellation(rep(5,3), rep(1, 125))
  
  parcellation2 <- brcbase::BrcParcellation(rep(5,3), 1:125)
  
  res <- hammingDistance(parcellation1, parcellation2)
  
  expect_true(res == 0)
})