context("Test distance between")

## test distanceBetween

test_that("it returns a distance object", {
  mat <- array(0, c(5,5,5))
  mat[1:2,1:2,1:2] <- 1
  mat[1:2,1:2,4:5] <- 2
  mat[1:2,4:5,4:5] <- 3
  mat[4:5,4:5,4:5] <- 4
  mat[3,3,3] <- 5
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  res <- distanceBetween(parcellation)
  expect_true(class(res) == "dist")
})

test_that("it returns the right values", {
  mat <- array(0, c(5,5,5))
  mat[1:2,1:2,1:2] <- 1
  mat[1:2,1:2,4:5] <- 2
  mat[1:2,4:5,4:5] <- 3
  mat[4:5,4:5,4:5] <- 4
  mat[3,3,3] <- 5
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  res <- distanceBetween(parcellation)
  
  sol.mat <- matrix(0, 5, 5)
  sol.mat[2,1] <- sum(abs(c(2,2,4)-c(2,2,2)))
  sol.mat[3,1] <- sum(abs(c(2,4,4)-c(2,2,2)))
  sol.mat[4,1] <- sum(abs(c(4,4,4)-c(2,2,2)))
  sol.mat[5,1] <- sum(abs(c(3,3,3)-c(2,2,2)))
  sol.mat[3,2] <- sum(abs(c(2,4,4)-c(2,2,4)))
  sol.mat[4,2] <- sum(abs(c(4,4,4)-c(2,2,4)))
  sol.mat[5,2] <- sum(abs(c(3,3,3)-c(2,2,4)))
  sol.mat[4,3] <- sum(abs(c(4,4,4)-c(2,4,4)))
  sol.mat[5,3] <- sum(abs(c(3,3,3)-c(2,4,4)))
  sol.mat[5,4] <- sum(abs(c(3,3,3)-c(4,4,4)))
  
  sol.mat <- sol.mat + t(sol.mat)
  
  expect_true(sum(abs(sol.mat - as.matrix(res))) < 1e-4)
})

######################

## test distance_manhattan

test_that("it returns the right distance when mat1 and mat2 are singular", {
  mat1 <- matrix(1:5, ncol = 1)
  mat2 <- matrix(6:10, ncol = 1)
  
  res <- distance_manhattan(mat1, mat2)
  expect_true(abs(res - sum(abs(1:5 - 6:10))) < 1e-4)
})

test_that("it returns the right distances for constant matrices", {
  mat1 <- matrix(rep(1:5, each = 5), 5, 5)
  mat2 <- matrix(rep(6:11, each = 5), 5, 6)
  
  res <- distance_manhattan(mat1, mat2)
  
  expect_true(all(dim(res) == c(6,5)))
  
  sol.mat <- matrix(0, 6, 5)
  for(i in 1:6){
    for(j in 1:5){
      sol.mat[i,j] <- 5*(i+5-j)
    }
  }
  expect_true(sum(abs(sol.mat - res)) < 1e-4)
})

#############################

## test .compute_parcellationDistance

test_that("it returns the correct minimum value", {
  mat <- array(0, c(5,5,5))
  mat[1:2,1:2,1:2] <- 1
  mat[1:2,1:2,4:5] <- 2
  mat[1:2,4:5,4:5] <- 3
  mat[4:5,4:5,4:5] <- 4
  mat[3,3,3] <- 5
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  res <- .compute_parcellationDistance(parcellation, 1, 2, dist = 
      distance_manhattan)
  
  expect_true(abs(res - sum(c(2,2,4)-c(2,2,2))) < 1e-4)
})