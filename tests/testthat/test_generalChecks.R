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

######################

## test .check.parcel

test_that("if parcel is NA, return all parcels", {
  partition <- 1:27
  res <- .check.parcel(NA, partition)
  
  expect_true(length(res) == 27)
  expect_true(all(res == 1:27))
})

test_that("if parcel is NA, returns all parcels in sorted order", {
  partition <- c(0,0,1,6,2,3,0,7,4,0,0,0,5,1)
  res <- .check.parcel(NA, partition)
  
  expect_true(length(res) == 8)
  expect_true(all(res == 0:7))
})

test_that("it allows the 0 parcel to be returned", {
  partition <- 0:10
  res <- .check.parcel(NA, partition)
  
  expect_true(length(res) == 11)
  expect_true(0 %in% res)
})

test_that("it fails when parcel is specified and invalid", {
  partition <- 1:27
  expect_error(.check.parcel(c(1,5,NA,15), partition))
  expect_error(.check.parcel(c(-1,5,2), partition))
  expect_error(.check.parcel(c(1,6,8.2), partition))
  expect_error(.check.parcel(c(1,5,40), partition))
  expect_error(.check.parcel(c(1,5,5), partition))
})

test_that("it fails when you ask for 0 but 0 is not in the parcellation", {
  partition <- 1:10
  expect_error(.check.parcel(0, partition))
})

test_that("it works when you ask for a valid parcel", {
  partition <- 1:27
  res <- .check.parcel(c(1,5,7), partition)
  
  expect_true(all(res == c(1,5,7)))
})

test_that("it works when you ask for a valid parcel that's 0 when there's 0",{
  partition <- c(0,0,1,6,3,2,6,4,5,0,0,2,0,5)
  res <- .check.parcel(c(0,1,5), partition)
  
  expect_true(all(res == c(0,1,5)))
})

test_that("it NOT order the parcels you ask for", {
  partition <- c(0,0,1,6,3,2,6,4,5,0,0,2,0,5)
  res <- .check.parcel(c(5,2,0,4), partition)
  
  expect_true(all(res == c(5,2,0,4)))
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