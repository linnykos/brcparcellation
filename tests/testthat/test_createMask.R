context("Test making a mask")

test_that("it fails for non-BrcParcellation objects", {
  expect_error(createMask(matrix(1:100,10,10)))
})

test_that("it fails for invalid BrcParcellations", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  parcellation$dim3d <- c(3,3,4)
  
  expect_error(createMask(parcellation))
})

test_that("it creates the right mask", {
  set.seed(10)
  vec <- sample(0:4, 125, replace = T)
  parcellation <- brcbase::BrcParcellation(rep(5,3), vec)
  
  res <- createMask(parcellation)
  expect_true(class(res) == "BrcParcellation")
  expect_true(all(res$dim3d == rep(5,3)))
  
  idx <- which(parcellation$partition != 0)
  expect_true(all(res$partition[idx] == 1))
  expect_true(all(res$partition[-idx] == 0))
})