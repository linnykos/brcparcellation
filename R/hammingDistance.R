#pairwise hamming distance
hammingDistance <- function(parcellation1, parcellation2, 
  numPairs = 5000){
  
  .is.BrcParcellation(parcellation1); .is.BrcParcellation(parcellation2)
  if(all(parcellation1$dim3d != parcellation2$dim3d)){
    stop("parcellation1 and parcellation2 must have the same dim3d")
  }
  
  idx1 <- which(parcellation1$partition != 0)
  idx2 <- which(parcellation2$partition != 0)
  idx <- base::union(idx1, idx2)
  
  pairs <- .generatePairs(numPairs, idx)
  
  vec <- apply(pairs, 1, function(x){
    bool1 <- (parcellation1$partition[x[1]] == parcellation1$partition[x[2]])
    bool2 <- (parcellation2$partition[x[1]] == parcellation2$partition[x[2]])
    
    bool1 == bool2
  })
  
  sum(vec)/nrow(pairs)
}

.generatePairs <- function(numPairs, vec){
  pairs <- cbind(sample(vec, numPairs, replace = T), 
    sample(vec, numPairs, replace = T))
  
  .checkPairs(pairs)
}

#currently: only removes pairs of points compared with themselves
.checkPairs <- function(mat){
  mat <- mat[-which(mat[,1] == mat[,2]),]
}