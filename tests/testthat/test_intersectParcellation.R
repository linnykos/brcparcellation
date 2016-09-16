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