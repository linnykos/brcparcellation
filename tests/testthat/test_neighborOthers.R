context("Test other forms of neighbors")

## test neighborVoxel2Parcel

test_that("it works on singleton parcellation", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  res <- neighborVoxel2Voxel(parcellation)
  res2 <- neighborVoxel2Parcel(parcellation)
  
  expect_true(length(res) == length(res2))
  for(i in 1:length(res)){
    expect_true(all(res[[i]] == res2[[i]]))
  }
})

test_that("it works on the slice parcellation", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), rep(1:3, each = 9))
  res <- neighborVoxel2Parcel(parcellation)
  
  expect_true(length(res) == 27)
  for(i in 1:9){
    expect_true(all(res[[i]] == c(1,2)))
  }
  for(i in 10:18){
    expect_true(all(res[[i]] == c(1,2,3)))
  }
  for(i in 19:27){
    expect_true(all(res[[i]] == c(2,3)))
  }
})

test_that("it fails not given a parcellation", {
  expect_error(neighborVoxel2Parcel(1:10))
})

test_that("it fails when given an invalid parcellation", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation$dim3d <- c(3,3,4)
  expect_error(neighborVoxel2Parcel(parcellation))
})

test_that("it fails when voxel is specified and invalid", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  expect_error(neighborVoxel2Parcel(parcellation, c(1,5,NA,15)))
  expect_error(neighborVoxel2Parcel(parcellation, c(-1,5,2)))
  expect_error(neighborVoxel2Parcel(parcellation, c(1,6,8.2)))
  expect_error(neighborVoxel2Parcel(parcellation, c(1,5,40)))
  expect_error(neighborVoxel2Parcel(parcellation, c(1,5,5)))
})


############################

## test neighborParcel2Voxel

test_that("it works on singleton partition", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  res <- neighborParcel2Voxel(parcellation)

  expect_true(length(res) == 27)
  expect_true(all(res[[1]] == c(1,2,4,5, 10, 11, 13, 14)))
  expect_true(all(res[[5]] == 1:18))
  expect_true(all(res[[14]] == 1:27))
  expect_true(all(res[[23]] == 10:27))
  expect_true(all(res[[27]] == c(14,15,17,18,23,24,26,27)))
})

test_that("the singleton parcellation is symmetric", {
  parcellation <- brcbase::BrcParcellation(c(4,4,4), 1:64)
  res <- neighborParcel2Voxel(parcellation)
  
  expect_true(length(res) == 64)
  for(i in 1:31){
    expect_true(length(res[[i]]) == length(res[[64-i+1]]))
  }
})

test_that("it works on slice partition", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), rep(1:3, each = 9))
  res <- neighborParcel2Voxel(parcellation)
  
  expect_true(length(res) == 3)
  expect_true(all(res[[1]] == 1:18))
  expect_true(all(res[[2]] == 1:27))
  expect_true(all(res[[3]] == 10:27))
})

test_that("it fails not given a parcellation", {
  expect_error(neighborParcel2Voxel(1:10))
})

test_that("it fails when given an invalid parcellation", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation$dim3d <- c(3,3,4)
  expect_error(neighborParcel2Voxel(parcellation))
})

test_that("it fails when voxel is specified and invalid", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), rep(1:3, each = 9))
  expect_error(neighborParcel2Voxel(parcellation, c(1,2,NA)))
  expect_error(neighborParcel2Voxel(parcellation, c(-1,2,3)))
  expect_error(neighborParcel2Voxel(parcellation, c(1,2,2.1)))
  expect_error(neighborParcel2Voxel(parcellation, c(1,2,4)))
  expect_error(neighborParcel2Voxel(parcellation, c(1,2,2)))
})


###############################

## test neighborParcel2Parcel

test_that("it works on slice partition", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), rep(1:3, each = 9))
  res <- neighborParcel2Parcel(parcellation)
  
  expect_true(length(res) == 3)
  expect_true(all(res[[1]] == 1:2))
  expect_true(all(res[[2]] == 1:3))
  expect_true(all(res[[3]] == 2:3))
})

test_that("it fails not given a parcellation", {
  expect_error(neighborParcel2Parcel(1:10))
})

test_that("it fails when given an invalid parcellation", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation$dim3d <- c(3,3,4)
  expect_error(neighborParcel2Parcel(parcellation))
})

test_that("it fails when voxel is specified and invalid", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), rep(1:3, each = 9))
  expect_error(neighborParcel2Parcel(parcellation, c(1,2,NA)))
  expect_error(neighborParcel2Parcel(parcellation, c(-1,2,3)))
  expect_error(neighborParcel2Parcel(parcellation, c(1,2,2.1)))
  expect_error(neighborParcel2Parcel(parcellation, c(1,2,4)))
  expect_error(neighborParcel2Parcel(parcellation, c(1,2,2)))
})


############################

## test .convertIdx2Parcel 

test_that("it behaves properly in the simplest setting", {
  vec <- c(1,5,2)
  partition <- c(0,0,0,1,2,3,4,5)
  expect_true(all(.convertIdx2Parcel(vec, partition) == c(0,2)))
})

test_that("it sorts the outcomes proplery", {
  set.seed(10)
  vec <- c(1,5,2)
  partition <- sample(10)
  expect_true(all(.convertIdx2Parcel(vec, partition) == 
      sort(.convertIdx2Parcel(vec, partition))))
})