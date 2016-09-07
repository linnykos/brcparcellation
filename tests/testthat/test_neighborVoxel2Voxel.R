context("Test findNeigbhors.R")

## test neighborShape_Box27()

test_that("it has 27 rows and 3 columns", {
  res <- neighborShape_Box27()
  expect_true(nrow(res) == 27)
  expect_true(ncol(res) == 3)
})

test_that("it has only 0, -1, and 1", {
  res <- neighborShape_Box27()
  expect_true(all(sort(unique(as.numeric(res))) == c(-1,0,1)))
})

test_that("it has unique rows", {
  res <- neighborShape_Box27()
  res <- res + 1
  vec <- apply(res, 1, function(x){x[1]*3^2+x[2]*3+x[3]})
  expect_true(length(unique(vec)) == 27)
})

## test .neighborShapeClosure

test_that("using box, it gives a proper box", {
  dim3d <- c(5,5,5)
  mat <- array(0, dim = dim3d)
  mat[2:4,2:4,2:4] <- 1
  res <- .neighborShapeClosure(dim3d, neighborShape_Box27())(c(3,3,3))
  expect_true(is.numeric(res))
  expect_true(length(res) == 27)
  expect_true(all(mat[res] == 1))
})

##########################

## test neighborVoxel2Voxel

test_that("it returns a list of the correct length, one per voxel",{
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  res <- neighborVoxel2Voxel(parcellation)
  
  expect_true(length(res) == 27)
})

test_that("it returns voxels that are indeed neighbors", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  res <- neighborVoxel2Voxel(parcellation)
  
  for(i in 1:length(res)){
    mat <- sapply(res[[i]], brcbase::voxelIdxTo3D, dim3d = parcellation$dim3d)
    loc <- brcbase::voxelIdxTo3D(parcellation$dim3d, 
      as.numeric(names(res)[i]))
    
    dif <- apply(mat, 2, function(x){x - loc})
    expect_true(all(dif >= -1))
    expect_true(all(dif <= 1))
    
    difColSum <- apply(dif, 2, function(x){sum(abs(x))})
    expect_true(any(difColSum == 0))
  }
})

test_that("it returns the voxel correctly when specified", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  res <- neighborVoxel2Voxel(parcellation)
  res2 <- neighborVoxel2Voxel(parcellation, voxel = c(5,10,15,20))
  
  expect_true(length(res2) == 4)
  expect_true(length(res) == 27)
  
  expect_true(all(res2[[1]] == res[[5]]))
  expect_true(all(res2[[2]] == res[[10]]))
  expect_true(all(res2[[3]] == res[[15]]))
  expect_true(all(res2[[4]] == res[[20]]))
})

test_that("voxels do not necessarily need to be in order", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  res <- neighborVoxel2Voxel(parcellation, c(1,2,6,10))
  res2 <- neighborVoxel2Voxel(parcellation, c(10,1,6,2))
  
  expect_true(all(res[[1]] == res2[[2]]))
  expect_true(all(res[[2]] == res2[[4]]))
  expect_true(all(res[[3]] == res2[[3]]))
  expect_true(all(res[[4]] == res2[[1]]))
})

test_that("the singleton parcellation is symmetric", {
  parcellation <- brcbase::BrcParcellation(c(4,4,4), 1:64)
  res <- neighborVoxel2Voxel(parcellation)
  
  expect_true(length(res) == 64)
  for(i in 1:31){
    expect_true(length(res[[i]]) == length(res[[64-i+1]]))
  }
})

test_that("it fails not given a parcellation", {
  expect_error(neighborVoxel2Voxel(1:10))
})

test_that("it fails when given an invalid parcellation", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation$dim3d <- c(3,3,4)
  expect_error(neighborVoxel2Voxel(parcellation))
})

test_that("it fails when voxel is specified and invalid", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  expect_error(neighborVoxel2Voxel(parcellation, c(1,5,NA,15)))
  expect_error(neighborVoxel2Voxel(parcellation, c(-1,5,2)))
  expect_error(neighborVoxel2Voxel(parcellation, c(1,6,8.2)))
  expect_error(neighborVoxel2Voxel(parcellation, c(1,5,40)))
  expect_error(neighborVoxel2Voxel(parcellation, c(1,5,5)))
})

test_that("it returns a proper list when testing for parcel 1 on the block",{
  mat <- array(0, rep(5,3))
  mat[2:4,2:4,2:4] <- 1
  mat[3,3,3] <- 2
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  idx <- which(parcellation$partition == 1)
  res <- neighborVoxel2Voxel(parcellation, voxel = idx)
  
  expect_true(length(res) == 26)
  expect_true(is.list(res))
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