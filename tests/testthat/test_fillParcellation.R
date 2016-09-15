context("Test fill Parcel")

test_that("it works on a simple singular-parcel case", {
  mat <- array(0, rep(3,3))
  mat[1:2, 1:2, 1:2] <- 1
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  res <- fillParcellation(parcellation)
  
  expect_true(class(res) == "BrcParcellation")
  expect_true(all(res$dim3d == rep(3,3)))
  expect_true(all(res$partition == 1))
})

test_that("it works on seperate block parcellation", {
  mat <- array(0, rep(5,3))
  mat[1:3, 1:3, 1:3] <- 1
  mat[4:5, 4:5, 4:5] <- 2
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  set.seed(10)
  res <- fillParcellation(parcellation)
  
  expect_true(class(res) == "BrcParcellation")
  expect_true(all(res$parcellation$dim3d == rep(5,3)))
  
  idx1 <- which(parcellation$partition == 1)
  expect_true(all(res$partition[idx1] == 1))
  
  idx2 <- which(parcellation$partition == 2)
  expect_true(all(res$partition[idx2] == 2))
  
  idx1.mat <- matrix(c(1,1,4, 1,2,4, 1,3,4, 1,4,4,
                       2,1,4, 2,2,4, 2,3,4, 2,4,4,
                       3,1,4, 3,2,4, 
                       1,4,1, 1,4,2, 1,4,3, 
                       2,4,1, 2,4,2, 2,4,3,
                       3,4,1, 3,4,2,
                       4,1,1, 4,1,2, 4,1,3, 4,1,4,
                       4,2,1, 4,2,2, 4,2,3, 4,2,4,
                       4,3,1, 4,3,2, 
                       1,1,5, 1,2,5, 1,3,5, 1,4,5,
                       2,1,5, 
                       3,1,5, 
                       1,5,1, 1,5,2, 1,5,3,
                       2,5,1, 2,5,2, 
                       3,5,1, 
                       5,1,1, 5,1,2, 5,1,3, 5,1,4,
                       5,2,1, 
                       5,3,1), ncol = 3, byrow = T)
  idx2.mat <- matrix(c(5,4,3, 4,5,3,
                       4,3,5, 3,4,5, 
                       4,5,3, 3,5,4,
                       5,4,3, 5,3,4,
                       5,5,3, 5,3,5, 3,5,5), ncol = 3, byrow = T)
  
  idx1.vec <- apply(idx1.mat, 1, brcbase::voxel3DToIdx, dim3d = rep(5,3))
  idx2.vec <- apply(idx2.mat, 1, brcbase::voxel3DToIdx, dim3d = rep(5,3))
  expect_true(all(res$partition[idx1.vec] == 1))
  expect_true(all(res$partition[idx2.vec] == 2))
})
