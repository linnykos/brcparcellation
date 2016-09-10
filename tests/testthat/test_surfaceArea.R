context("Test Surface Area")

test_that("it works as normal", {
  mat <- array(0, rep(5,3))
  mat[2:4,2:4,2:4] <- 1
  mat[3,3,3] <- 2
  parcellation <- brcbase::buildBrcParcellation(mat)
  
  res <- surfaceArea(parcellation)
  
  expect_true(length(res) == 2)
  expect_true(all(res == c(26,1)))
})