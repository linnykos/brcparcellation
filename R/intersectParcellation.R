#' Intersect two parcellations
#' 
#' This function creates a new parcellation from two existing parcellations
#' where voxels in the new parcellation are in the same parcel if and only if
#' the voxels were in the same parcels within each of the two input parcellations.
#'
#' @param parcellation1 a \code{BrcParcellation} object
#' @param parcellation2 a \code{BrcParcellation} object
#'
#' @return a \code{BrcParcellation} object
#' @export
intersectParcellation <- function(parcellation1, parcellation2){
  .is.BrcParcellation(parcellation1); .is.BrcParcellation(parcellation2)
  if(all(parcellation1$dim3d != parcellation2$dim3d)){
    stop("parcellation1 and parcellation2 must have the same dim3d")
  }
  
  dim3d <- parcellation2$dim3d
  vec1 <- parcellation1$partition; vec2 <- parcellation2$partition
  
  if(all(vec1 == vec2)) return(parcellation1)
  num.uniq1 <- length(unique(vec1)); num.uniq2 <- length(unique(vec2))
  if(num.uniq1 == prod(dim3d)) return(parcellation1)
  if(num.uniq2 == prod(dim3d)) return(parcellation2)
  
  vec2 <- vec2 * (max(vec1)+1)
  vec <- vec1 + vec2
  vec <- .reindex(vec)
  
  if(length(unique(vec)) == num.uniq1) return(parcellation1)
  if(length(unique(vec)) == num.uniq2) return(parcellation2)
  
  brcbase::BrcParcellation(dim3d = dim3d, partition = vec)
}

.reindex <- function(vec){
  uniq <- unique(vec)
  
  if(0 %in% uniq) uniq <- uniq[uniq != 0]
  
  plyr::mapvalues(vec, from = uniq, to = 1:length(uniq), warn_missing = FALSE)
}