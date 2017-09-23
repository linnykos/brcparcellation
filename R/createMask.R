#' Create a mask from an existing parcellation
#' 
#' This function takes in a \code{BrcParcellation} object and returns
#' a new \code{BrcParcellation} object where all the voxels assigned
#' to any parcels in the input parcellation are now assigned to parcel 1.
#' 
#' This is useful if the user wants to make a mask, so downstream, the user
#' can zero-out all the voxels not in the parcellation for a region-of-interest
#' (RoI) study.
#'
#' @param parcellation  a \code{BrcParcellation} object
#'
#' @return a \code{BrcParcellation} object
#' @export
createMask <- function(parcellation){
  .is.BrcParcellation(parcellation)
  
  parcellation$partition[parcellation$partition != 0] <- 1
  
  parcellation
}