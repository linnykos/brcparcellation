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

## test .findNeighborVoxels

test_that("it returns a list of the correct length, one per voxel",{
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  res <- .findNeighborVoxels(parcellation)
  
  expect_true(length(res) == 27)
})

test_that("it returns voxels that are indeed neighbors", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  res <- .findNeighborVoxels(parcellation)
  
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

############################

## test .convertIdx2Parcel 

test_that("it behaves properly in the simplest setting", {
  vec <- c(1,5,2)
  partition <- c(0,0,0,1,2,3,4,5)
  expect_true(all(.convertIdx2Parcel(vec, partition) == c(0,2)))
})