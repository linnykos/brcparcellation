context("Test other forms of neighbors")

## test neighborVoxel2Parcel

test_that("it works on singleton parcellation", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), 1:27)
  res <- neighborVoxel2Voxel(parcellation)
  res2 <- neighborVoxel2Parcel(parcellation)
  
  expect_true(length(res) == length(res2))
  for(i in 1:length(res)){
    expect_true(all(res[[i]] == res2[[i]]))
  }
})

test_that("it works on the slice parcellation", {
  parcellation <- brcbase::BrcParcellation(c(3,3,3), rep(1:3, each = 9))
  res <- neighborVoxel2Parcel(parcellation)
  
  expect_true(length(res) == 27)
  for(i in 1:9){
    expect_true(all(res[[i]] == c(1,2)))
  }
  for(i in 10:18){
    expect_true(all(res[[i]] == c(1,2,3)))
  }
  for(i in 19:27){
    expect_true(all(res[[i]] == c(2,3)))
  }
})

############################

## test .convertIdx2Parcel 

test_that("it behaves properly in the simplest setting", {
  vec <- c(1,5,2)
  partition <- c(0,0,0,1,2,3,4,5)
  expect_true(all(.convertIdx2Parcel(vec, partition) == c(0,2)))
})

test_that("it sorts the outcomes proplery", {
  set.seed(10)
  vec <- c(1,5,2)
  partition <- sample(10)
  expect_true(all(.convertIdx2Parcel(vec, partition) == 
      sort(.convertIdx2Parcel(vec, partition))))
})