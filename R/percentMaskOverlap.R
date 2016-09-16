percentMaskOverlap <- function(parcellation1, parcellation2){
  .is.BrcParcellation(parcellation1); .is.BrcParcellation(parcellation2)
  if(all(parcellation1$dim3d != parcellation2$dim3d)){
    stop("parcellation1 and parcellation2 must have the same dim3d")
  }
  
  idx1 <- which(parcellation1$partition != 0)
  idx2 <- which(parcellation2$partition != 0)
  
  length(base::intersect(idx1, idx2)) / length(base::union(idx1, idx2))
}