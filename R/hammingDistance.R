#pairwise hamming distance
hammingDistance <- function(parcellation1, parcellation2, 
  numPairs = 5000){
  
  .is.BrcParcellation(parcellation1); .is.BrcParcellation(parcellation2)
  if(all(parcellation1$dim3d != parcellation2$dim3d)){
    stop("parcellation1 and parcellation2 must have the same dim3d")
  }
  
  dim3d <- parcellation1$dim3d
  pairs <- .generatePairs(numPairs, dim3d)
  mat <- .convertIntoPairs(pairs, prod(dim3d))
  
  vec <- apply(mat, 2, function(x){
    bool1 <- (parcellation1$partition[x[1]] == parcellation1$partition[x[2]])
    bool2 <- (parcellation2$partition[x[1]] == parcellation2$partition[x[2]])
    
    bool1 == bool2
  })
  
  sum(vec)/ncol(mat)
}

.generatePairs <- function(numPairs, dim3d){
  d <- prod(dim3d)
  vec <- 1:d^2
  remove.points <- (1:d) + (0:(d-1))*d
  vec <- vec[-remove.points]
  
  if(length(numPairs) > length(vec)){
    vec
  } else {
    sample(vec, numPairs, replace = F)
  }
}

.convertIntoPairs <- function(vec, d){
  row1 <- ceiling(vec/ d)
  row2 <- vec %% d
  row2[row2 == 0] <- d
  rbind(row1, row2)
}