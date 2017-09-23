#' Compute the surface area of each parcel
#' 
#' This function computes the surface area for each parcel (i.e.,
#' how many voxels are on the surface of a parcel) and returns it as a 
#' vector, one number for each parcel. 
#' 
#' The definition of which voxel lie
#' on the surface is dictated by \code{shape.mat}, where a voxel is 
#' on the surface if one of its neighbors belong to a different parcel or
#' one of its neighbor is an empty voxel.
#'
#' @param parcellation a \code{BrcParcellation} object
#' @param parcel a vector of indicies of which parcels to compute the surface area for
#' @param shape.mat the output of \code{neighborShape} function, such as \code{neighborShape_Box27()}
#'
#' @return a vector of numerics
#' @export
surfaceArea <- function(parcellation, parcel = NA,
  shape.mat = neighborShape_Box27()){
   .is.BrcParcellation(parcellation)
  parcel <- .check.parcel(parcel, parcellation$partition)
  if(any(parcel == 0)) parcel <- parcel[parcel != 0]
 
  voxel <- which(parcellation$partition %in% parcel)
  parcel.mapping <- parcellation$partition[voxel]
  
  on.boundary <- .multipleParcelPerVoxel(parcellation, voxel, shape.mat)
  
  surface.vec <- sapply(parcel, function(x){
    sum(on.boundary[which(parcel.mapping == x)])
  })
  names(surface.vec) <- parcel
  
  surface.vec
}

#' Compute the surface area of each parcel (percentage)
#' 
#' This function computes the surface area percentage for each parcel (i.e.,
#' how many voxels are on the surface of a parcel) and returns it as a 
#' vector, one number for each parcel. The percentage is defined as
#' the number of voxels in the parcel that lie on the surface divided
#' by the number of voxels in the parcel.
#' 
#' The definition of which voxel lie
#' on the surface is dictated by \code{shape.mat}, where a voxel is 
#' on the surface if one of its neighbors belong to a different parcel or
#' one of its neighbor is an empty voxel.
#'
#' @param parcellation a \code{BrcParcellation} object
#' @param parcel a vector of indicies of which parcels to compute the surface area for
#' @param shape.mat the output of \code{neighborShape} function, such as \code{neighborShape_Box27()}
#'
#' @return a vector of numerics
#' @export
surfaceAreaNeighborPercentage <- function(parcellation, parcel = NA, 
  shape.mat = neighborShape_Box27()){
  
  .is.BrcParcellation(parcellation)
  parcel <- .check.parcel(parcel, parcellation$partition)
  if(any(parcel == 0)) parcel <- parcel[parcel != 0]
  
  res <- neighborParcel2Voxel(parcellation, parcel, shape.mat)
  
  percentage <- lapply(1:length(res), function(x){
    vec <- res[[x]]
    vec <- vec[parcellation$partition[vec] != as.numeric(names(res)[x])]
    tab <- table(parcellation$partition[vec])
    tab/sum(tab)
  })
  
  names(percentage) <- parcel
  percentage
}

.multipleParcelPerVoxel <- function(parcellation, voxel, shape.mat){
  res <- neighborVoxel2Voxel(parcellation, voxel = voxel, 
    shape.mat = shape.mat)
  
  sapply(res, function(x){
    if(length(unique(parcellation$partition[x])) > 1) { return(TRUE) } else
      return(FALSE)
  })
}
