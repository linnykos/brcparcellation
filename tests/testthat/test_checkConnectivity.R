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