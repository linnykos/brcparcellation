context("Test fill Parcel")

test_that("it works on a simple singular-parcel case", {
  mat <- array(0, rep(3,3))
  mat[1:2, 1:2, 1:2] <- 1
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  res <- fillParcellation(parcellation)
  
  expect_true(class(res) == "BrcParcellation")
  expect_true(all(res$parcellation$dim3d == rep(3,3)))
  expect_true(all(res$parcellation$partition == 1))
})