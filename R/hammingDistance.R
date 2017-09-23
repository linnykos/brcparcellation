#' Compute the pairwise distance between two parcellations: Pairwise hamming distance
#' 
#' Given two \code{BrcParcellation} objects, \code{parcellation1} and
#' \code{parcellation2}, compute the pairwise Hamming distance. This is defined the 
#' following way: for random pairs of voxels (which are assigned to voxels in
#' both parcellations), count a distance of 0 if both voxels are either in the same
#' parcel within each parcellation or if both voxels are in different parcels 
#' within each parcellation, but count a distance of 1 if both voxels are in the
#' same parcel in one parcellation but in different parcels in the other parcellation.
#' The distance function then is the average of these (0,1) values over all the
#' pairs. 
#' 
#' This distance is more accurate when \code{numPairs} is larger, at the cost
#' of more computational time. 
#'
#' @param parcellation1 a \code{BrcParcellation} object
#' @param parcellation2 a \code{BrcParcellation} object
#' @param numPairs number of pairs of voxels to compute the pairwise hamming distance over
#'
#' @source Ben-David, Shai, Ulrike Von Luxburg, and Dávid Pál. 
#' "A sober look at clustering stability." International Conference on 
#' Computational Learning Theory. Springer, Berlin, Heidelberg, 2006, (Definition 4)
#' 
#' @return a numeric
#' @export
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