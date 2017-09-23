#' Fills all the empty voxels in a parcellation 
#' 
#' If a parcellation (\code{BrcParcellation} object) has empty voxels,
#' this function returns a new parcellation such that every empty voxel is
#' assigned to same parcel as its nearest non-empty voxel. If an empty 
#' voxel is equidistant to more than one non-empty nearest voxel that differ
#' in parcel assignments, the empty voxel is randomly assigned to one of the
#' possible parcels.
#' 
#' As all parcellations represent 3-dimensional objects (given by
#' \code{parcellation$dim3d}), this function will assign all the empty voxel
#' in the 3-dimensional space to a parcel.
#'
#' @param parcellation a \code{BrcParcellation} object
#' @param shape.mat the output of \code{neighborShape} function, such as \code{neighborShape_Box27()}
#'
#' @return a \code{BrcParcellation} object 
#' @export
fillParcellation <- function(parcellation, shape.mat = neighborShape_Box27()){
  .is.BrcParcellation(parcellation)
 
  empty.voxel <- which(parcellation$partition == 0)

  while(length(empty.voxel) != 0){
    res <- neighborVoxel2Parcel(parcellation, voxel = empty.voxel,
      shape.mat = shape.mat)
    
    vec <- sapply(res, function(x){
      if(any(x != 0)){
        x <- x[x!=0]
        if(length(x) == 1) x <- rep(x,2)
        return(sample(x,1)) #written to deal with sample(integer, size)
      } else {return(0)}
    })
    
    parcellation$partition[empty.voxel[vec != 0]] <- vec[vec != 0]
    empty.voxel <- empty.voxel[vec == 0]
  }

  parcellation
}