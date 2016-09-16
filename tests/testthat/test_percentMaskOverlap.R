context("Test Percent Mask Overlap")

test_that("it returns 1 for parcellation with itself", {
  set.seed(10)
  vec <- sample(0:4, 125, replace = T)
  parcellation <- brcbase::BrcParcellation(rep(5,3), vec)
  
  res <- percentMaskOverlap(parcellation, parcellation)
  
  expect_true(res == 1)
})

test_that("it returns 1 for parcellation and its subparcellation", {
  set.seed(10)
  vec <- sample(0:5, 125, replace = T)
  vec2 <- vec
  idx <- which(vec2 == 1)
  vec2[idx] <- sample(c(1,6), length(idx), replace = T)
  
  parcellation1 <- brcbase::BrcParcellation(rep(5,3), vec)
  parcellation2 <- brcbase::BrcParcellation(rep(5,3), vec2)
  
  res <- percentMaskOverlap(parcellation1, parcellation2)
  
  expect_true(res == 1)
})

test_that("it returns 1 when you relabel the parcellation", {
  set.seed(10)
  vec <- sample(0:4, 125, replace = T)
  parcellation1 <- brcbase::BrcParcellation(rep(5,3), vec)
  
  vec2 <- plyr::mapvalues(vec, from = 1:4, c(3,1,4,2))
  parcellation2 <- brcbase::BrcParcellation(rep(5,3), vec2)
  
  res <- percentMaskOverlap(parcellation1, parcellation2)
  
  expect_true(res == 1)
})

test_that("it returns 0 when you complement the parcellation", {
  set.seed(10)
  vec <- sample(0:1, 64, replace = T)
  parcellation1 <- brcbase::BrcParcellation(rep(4,3), vec)
  
  vec2 <- 1-vec
  parcellation2 <- brcbase::BrcParcellation(rep(4,3), vec2)
  
  res <- percentMaskOverlap(parcellation1, parcellation2)
  
  expect_true(res == 0)
})

test_that("it can return exactly 0.5", {
  mat <- array(1, rep(4,3))
  mat2 <- array(c(rep(0, 32), rep(1, 32)), rep(4,3))
  
  parcellation1 <- brcbase::buildBrcParcellation(mat)
  parcellation2 <- brcbase::buildBrcParcellation(mat2)
  
  res <- percentMaskOverlap(parcellation1, parcellation2)
  
  expect_true(abs(res - 0.5) < 1e-6)
})

test_that("it returns a value between 0 and 1", {
  set.seed(10)
  vec <- sample(0:5, 64, replace = T)
  parcellation1 <- brcbase::BrcParcellation(rep(4,3), vec)
  
  vec2 <- sample(0:5, 64, replace = T)
  parcellation2 <- brcbase::BrcParcellation(rep(4,3), vec2)
  
  res <- percentMaskOverlap(parcellation1, parcellation2)
  
  expect_true(res > 0)
  expect_true(res < 1)
})


test_that("it fails for non-BrcParcellation objects", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  
  expect_error(percentMaskOverlap(matrix(1:100,10,10), parcellation))
  expect_error(percentMaskOverlap(parcellation, matrix(1:100,10,10)))
})

test_that("it fails for invalid BrcParcellations", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation$dim3d <- c(3,3,4)
  parcellation2 <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  
  expect_error(percentMaskOverlap(parcellation, parcellation2))
  expect_error(percentMaskOverlap(parcellation2, parcellation))
})

test_that("it does not work for parcellations of different sizes", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation2 <- brcbase::BrcParcellation(c(4,4,4), 1:64)
  
  expect_error(percentMaskOverlap(parcellation, parcellation2))
})