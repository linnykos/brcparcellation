context("Test intersect Parcellation")

## test intersectParcellation

test_that("intersection of a parcellation with itself returns the same",{
  set.seed(10)
  vec <- sample(1:5, 64, replace = T)
  parcellation <- brcbase::BrcParcellation(rep(4,3), vec)
  
  res <- intersectParcellation(parcellation, parcellation)
  
  expect_true(all(res$dim3d == parcellation$dim3d))
  expect_true(all(res$partition == parcellation$partition))
})

test_that("intersection of a parcellation with singleton returns singleton",{
  set.seed(10)
  vec <- sample(1:5, 64, replace = T)
  parcellation1 <- brcbase::BrcParcellation(rep(4,3), vec)
  parcellation2 <- brcbase::BrcParcellation(rep(4,3), 1:64)
  
  res <- intersectParcellation(parcellation1, parcellation2)
  
  expect_true(all(res$dim3d == parcellation1$dim3d))
  expect_true(all(res$partition == parcellation2$partition))
  
  res2 <- intersectParcellation(parcellation1, parcellation2)
  expect_true(all(res2$partition == parcellation2$partition))
})


test_that("it works on a typical intersection", {
  mat1 <- array(0, rep(3,3))
  mat1[,,1] <- 1
  parcellation1 <- brcbase::buildBrcParcellation(mat1)
  
  mat2 <- array(0, rep(3,3))
  mat2[,1,] <- 1
  parcellation2 <- brcbase::buildBrcParcellation(mat2)
  
  res <- intersectParcellation(parcellation1, parcellation2)
  expect_true(all(res$dim3d == rep(3,3)))
  expect_true(length(unique(res$partition)) == 4)
  
  idx1 <- unique(res$partition[1:3])
  idx2 <- unique(res$partition[4:9])
  idx3 <- unique(res$partition[c(10:12, 19:21)])
  idx4 <- unique(res$partition[-c(1:12, 19:21)])
  
  expect_true(length(idx1) == 1)
  expect_true(length(idx2) == 1)
  expect_true(length(idx3) == 1)
  expect_true(length(idx4) == 1)
  
  expect_true(idx1 != idx2)
  expect_true(idx1 != idx3)
  expect_true(idx2 != idx3)
  expect_true(idx4 == 0)
})

test_that("subset par. of a parcellation returns the exact subset par.",{
  set.seed(10)
  vec <- sample(0:5, 125, replace = T)
  vec2 <- vec
  idx <- which(vec2 == 0)
  vec2[idx] <- sample(c(0,6), length(idx), replace = T)
  
  parcellation1 <- brcbase::BrcParcellation(rep(5,3), vec)
  parcellation2 <- brcbase::BrcParcellation(rep(5,3), vec2)
  
  res <- intersectParcellation(parcellation1, parcellation2)
  expect_true(all(res$partition == vec2))
  
  res2 <- intersectParcellation(parcellation2, parcellation1)
  expect_true(all(res2$partition == vec2))
})

test_that("it fails for non-BrcParcellation objects", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  
  expect_error(intersectParcellation(matrix(1:100,10,10), parcellation))
  expect_error(intersectParcellation(parcellation, matrix(1:100,10,10)))
})

test_that("it fails for invalid BrcParcellations", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation$dim3d <- c(3,3,4)
  parcellation2 <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  
  expect_error(intersectParcellation(parcellation, parcellation2))
  expect_error(intersectParcellation(parcellation2, parcellation))
})

test_that("it does not work for parcellations of different sizes", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation2 <- brcbase::BrcParcellation(c(4,4,4), 1:64)
  
  expect_error(intersectParcellation(parcellation, parcellation2))
})

##################################################

## test .reindex

test_that("it works as desired", {
  set.seed(10)
  vec <- sample(c(0,1,5,19,21:30), 200, replace = T)
  
  res <- .reindex(vec)
  
  expect_true(all(which(vec == 0) == which(res == 0)))
  
  uniq <- unique(vec)
  expect_true(length(unique(res)) == length(uniq))
  
  for(i in uniq){
    idx <- which(vec == i)
    expect_true(length(unique(res[idx])) == 1)
  }
})