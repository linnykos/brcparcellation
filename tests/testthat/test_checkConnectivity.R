context("Test connectivity")

test_that("it works properly", {
  mat <- array(0, rep(5,3))
  mat[2:4,2:4,2:4] <- 1
  mat[3,3,3] <- 2
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  res <- checkConnectivity(parcellation)
  
  expect_true(length(res) == 2)
  expect_true(all(res))
  expect_true(all(names(res) == as.character(1:2)))
})

test_that("it returns not connected", {
  parcellation <- brcbase::BrcParcellation(rep(3,3), c(1,rep(2,25),1))
  res <- checkConnectivity(parcellation)
  
  expect_true(length(res) == 2)
  expect_true(all(res == c(FALSE,TRUE)))
  expect_true(all(names(res) == as.character(1:2)))
})


test_that("it fails for non-BrcParcellation objects", {
  expect_error(checkConnectivity(matrix(1:100,10,10)))
})

test_that("it fails for invalid BrcParcellations", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation$dim3d <- c(3,3,4)
  expect_error(checkConnectivity(parcellation))
})

test_that("it returns an area for 1 for singleton parcellations", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  res <- checkConnectivity(parcellation)
  
  expect_true(length(res) == 27)
  expect_true(all(res))
  expect_true(all(names(res) == as.character(1:27)))
})

test_that("it does not change the order of parcellations I pass in", {
  mat <- array(1, rep(5,3))
  mat[2:4,2:4,2:4] <- 3
  mat[3,3,3] <- 2
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  res <- checkConnectivity(parcellation)
  res2 <- checkConnectivity(parcellation, parcel = c(3,1,2))
  
  expect_true(res[1] == res2[2])
  expect_true(res[2] == res2[3])
  expect_true(res[3] == res2[1])
})


test_that("the connectivity doesn't change if I rotate the parcellation", {
  set.seed(10)
  partition <- sample(1:3, 64, replace = T) 
  parcellation <- brcbase::BrcParcellation(c(4,4,4), partition)
  res <- checkConnectivity(parcellation)
  
  partition2 <- rev(partition)
  parcellation2 <- brcbase::BrcParcellation(c(4,4,4), partition2)
  res2 <- checkConnectivity(parcellation2)
  
  expect_true(all(res == res2))
})

test_that("it is affected by shape.mat", {
  mat <- array(0, rep(5,3))
  mat[2:4,2:4,2:4] <- 1
  mat[1,1,1] <- 1
  parcellation <- brcbase::buildBrcParcellation(mat)
  res <- checkConnectivity(parcellation)
  res2 <- checkConnectivity(parcellation, shape.mat = neighborShape_Diamond7())
  
  expect_true(res)
  expect_true(!res2)
})