context("Test general checks")

## test .is.BrcParcellation

test_that("it fails not given a parcellation", {
  expect_error(.is.BrcParcellation(1:10))
})

test_that("it fails when given an invalid parcellation", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation$dim3d <- c(3,3,4)
  expect_error(.is.BrcParcellation(parcellation))
})

test_that("it works on normal parcellation", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  expect_true(.is.BrcParcellation(parcellation))
})

############################

## test .formGrid

test_that("it forms the right grid", {
  res <- .formGrid(c(3,4,5))
  
  expect_true(ncol(res) == 3)
  expect_true(nrow(res) == prod(3:5))
  
  idx <- apply(res, 1, brcbase::voxel3DToIdx, dim3d = c(3,4,5))
  expect_true(all(sort(idx) == 1:prod(3:5)))
})