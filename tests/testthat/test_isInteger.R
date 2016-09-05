context("Test isInteger")

test_that("it errors on NAs with numerics", {
  expect_error(.is.nonNegInteger(c(3, NA, 5)))
  expect_error(.is.nonNegInteger(c(NA, 3, 5)))
})

test_that("it works on numerics",{
  expect_true(.is.nonNegInteger(1:10))
})

test_that("it errors on negatives", {
  expect_error(.is.nonNegInteger(-1:5))
})

test_that("it errors on decimals", {
  expect_error(.is.nonNegInteger(1:10+.5))
  expect_error(.is.nonNegInteger(c(1,6,4,2.4,2)))
})