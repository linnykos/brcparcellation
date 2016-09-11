context("Test Surface Area")

test_that("it works as normal", {
  mat <- array(0, rep(5,3))
  mat[2:4,2:4,2:4] <- 1
  mat[3,3,3] <- 2
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  res <- surfaceArea(parcellation)
  
  expect_true(length(res) == 2)
  expect_true(all(res == c(26,1)))
  expect_true(all(names(res) == as.character(1:2)))
})

test_that("it fails for non-BrcParcellation objects", {
  expect_error(surfaceArea(matrix(1:100,10,10)))
})

test_that("it fails for invalid BrcParcellations", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation$dim3d <- c(3,3,4)
  expect_error(surfaceArea(parcellation))
})

test_that("it returns an area for 1 for singleton parcellations", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  res <- surfaceArea(parcellation)
  
  expect_true(length(res) == 27)
  expect_true(all(res == 1))
  expect_true(all(names(res) == as.character(1:27)))
})

test_that("it does not change the order of parcellations I pass in", {
  mat <- array(1, rep(5,3))
  mat[2:4,2:4,2:4] <- 3
  mat[3,3,3] <- 2
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  res <- surfaceArea(parcellation)
  res2 <- surfaceArea(parcellation, parcel = c(3,1,2))
  
  expect_true(res[1] == res2[2])
  expect_true(res[2] == res2[3])
  expect_true(res[3] == res2[1])
})

test_that("the surface area is less than the volume", {
  set.seed(10)
  partition <- sample(1:3, 27, replace = T) 
  parcellation <- brcbase::BrcParcellation(c(3,3,3), partition)
  res <- surfaceArea(parcellation)
  
  for(i in 1:3){
    expect_true(res[i] <= length(which(partition == i)))
  }
})

test_that("the surface area doesn't change if I rotate the parcellation", {
  set.seed(10)
  partition <- sample(1:3, 64, replace = T) 
  parcellation <- brcbase::BrcParcellation(c(4,4,4), partition)
  res <- surfaceArea(parcellation)
  
  partition2 <- rev(partition)
  parcellation2 <- brcbase::BrcParcellation(c(4,4,4), partition2)
  res2 <- surfaceArea(parcellation2)
  
  expect_true(all(res == res2))
})

test_that("I can correctly set the parcel", {
  set.seed(10)
  partition <- sample(1:3, 64, replace = T) 
  parcellation <- brcbase::BrcParcellation(c(4,4,4), partition)
  res <- surfaceArea(parcellation)
  res2 <- surfaceArea(parcellation, parcel = 3)
  
  expect_true(res[3] == res2)
})

test_that("it correctly computes the surface area of a block parcellation", {
  mat <- array(0, rep(5,3))
  mat[1:3, 1:3, 1:3] <- 1
  mat[4:5, 4:5, 4:5] <- 2
  parcellation <- brcbase::buildBrcParcellation(mat)
  res <- surfaceArea(parcellation)
  
  expect_true(length(res) == 2)
  expect_true(all(names(res) == as.character(c(1:2))))
  expect_true(all(res == c(27-8, 8-1)))
})