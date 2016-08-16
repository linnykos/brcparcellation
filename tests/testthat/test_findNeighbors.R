context("Test findNeigbhors.R")

## test neighborShape_Box27()

test_that("it has 27 rows and 3 columns", {
  res <- neighborShape_Box27()
  expect_true(nrow(res) == 27)
  expect_true(ncol(res) == 3)
})

test_that("it has only 0, -1, and 1", {
  res <- neighborShape_Box27()
  expect_true(all(sort(unique(as.numeric(res))) == c(-1,0,1)))
})

test_that("it has unique rows", {
  res <- neighborShape_Box27()
  res <- res + 1
  vec <- apply(res, 1, function(x){x[1]*3^2+x[2]*3+x[3]})
  expect_true(length(unique(vec)) == 27)
})

## test .neighborShapeClosure

test_that("using box, it gives a proper box", {
  dim3d <- c(5,5,5)
  mat <- array(0, dim = dim3d)
  mat[2:4,2:4,2:4] <- 1
  res <- .neighborShapeClosure(dim3d, neighborShape_Box27())(c(3,3,3))
  expect_true(is.numeric(res))
  expect_true(length(res) == 27)
  expect_true(all(mat[res] == 1))
})