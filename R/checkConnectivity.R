#' Check the connectivity of parcels in a parcellation
#' 
#' This function checks to see if each parcel in a parcellation are connected.
#' This can be useful is one wants ensure that no parcel consists of two
#' disjoint sets of voxels. 
#' 
#' By default, \code{parcel} is set to \code{NA}, meaning this function will check
#' every parcel. 
#' 
#' This function returns a vector of \code{TRUE}, \code{FALSE} values, one for
#' each parcel.
#'
#' @param parcellation a \code{BrcParcellation} object
#' @param parcel a vector of indicies of which parcels to check the connectivity of
#' @param shape.mat the output of \code{neighborShape} function, such as \code{neighborShape_Box27()}
#'
#' @return a vector of booleans
#' @export
checkConnectivity <- function(parcellation, parcel = NA,
  shape.mat = neighborShape_Box27()){
   .is.BrcParcellation(parcellation)
  parcel <- .check.parcel(parcel, parcellation$partition)
  if(any(parcel == 0)) parcel <- parcel[parcel != 0]
  
  voxel <- which(parcellation$partition %in% parcel)
  parcel.mapping <- parcellation$partition[voxel]
  
  connected.bool <- .checkNeighborSameParcelPerVoxel(parcellation, voxel, 
    shape.mat)
  
  connected <- sapply(parcel, function(x){
    idx <- which(parcel.mapping == x)
    if(length(idx) == 1) return(TRUE)
    all(connected.bool[idx]) 
  })
  names(connected) <- parcel
  
  connected
}

.checkNeighborSameParcelPerVoxel <- function(parcellation, voxel, shape.mat){
  res <- neighborVoxel2Voxel(parcellation, voxel = voxel, 
    shape.mat = shape.mat)
  
  sapply(1:length(res), function(x){
    if(length(which(parcellation$partition[res[[x]]] == 
        parcellation$partition[voxel[x]])) <= 1) { 
      return(FALSE) 
    } else
      return(TRUE)
  })
}