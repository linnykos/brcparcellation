#' Find neighboring parcels for each voxel
#' 
#' Given a \code{BrcParcellation} object and a vector of voxels
#' indices in \code{voxel}, find all the parcels that neighbor each of
#' the voxels. By default, \code{voxel} is set to \code{NA}, meaning
#' it finds the neighbor of all voxels.
#' 
#' The \code{voxel} vector are indicies referring of specific locations in
#' \code{parcellation}. They must be positive integers between \code{1} and 
#' \code{length(parcellation$partition)}.
#' 
#' The output refer to specific parcels and range between \code{1} and
#' \code{max(parcellation$partition)}.
#'
#' @param parcellation a \code{BrcParcellation} object
#' @param voxel a vector of indicies of which voxels to select the neighbors of
#' @param shape.mat the output of \code{neighborShape} function, such as \code{neighborShape_Box27()}
#'
#' @return A list of integers, one vector per voxel
#' @export
neighborVoxel2Parcel <- function(parcellation, voxel = NA,
  shape.mat = neighborShape_Box27()){
  
  res <- neighborVoxel2Voxel(parcellation, voxel, shape.mat)
  res <- lapply(res, .convertIdx2Parcel, partition = parcellation$partition)
  
  res
}

#' Find neighboring voxels for each parcel
#' 
#' Given a \code{BrcParcellation} object and a vector of parcel
#' indicies in \code{parcel}, find all voxels that neighbor each of the
#' \code{parcel}.By default, \code{parcel} is set to \code{NA}, meaning
#' it finds the neighbor of all parcel
#' 
#' The \code{parcel} vector must be positive integers ranging between
#' \code{1} and \code{max(parcellation$partition)}.
#' 
#' The voxel vector are indicies referring of specific locations in
#' \code{parcellation}. They are positive integers between \code{1} and 
#' \code{length(parcellation$partition)}.
#'
#' @param parcellation a \code{BrcParcellation} object
#' @param parcel a vector of indicies of which parcels to select the neighbors of
#' @param shape.mat the output of \code{neighborShape} function, such as \code{neighborShape_Box27()}
#'
#' @return A list of integers, one vector per parcel
#' @export
neighborParcel2Voxel <- function(parcellation, parcel = NA,
  shape.mat = neighborShape_Box27()){
  .is.BrcParcellation(parcellation)
  parcel <- .check.parcel(parcel, parcellation$partition)
  
  res <- lapply(1:length(parcel), function(x){
    idx <- which(parcellation$partition == parcel[x])
    voxelNeigh <- neighborVoxel2Voxel(parcellation, voxel = idx,
      shape.mat)
    sort(unique(unlist(voxelNeigh)))
  })
  
  names(res) <- parcel
  res
}

#' Find neighboring parcels for each parcel
#' 
#' Given a \code{BrcParcellation} object and a vector of parcel
#' indicies in \code{parcel}, find all parcels that neighbor each of the
#' \code{parcels}.By default, \code{parcel} is set to \code{NA}, meaning
#' it finds the neighbor of all parcels.
#' 
#' The \code{parcel} vector must be positive integers ranging between
#' \code{1} and \code{max(parcellation$partition)}.
#' 
#' @param parcellation a \code{BrcParcellation} object
#' @param parcel a vector of indicies of which parcels to select the neighbors of
#' @param shape.mat the output of \code{neighborShape} function, such as \code{neighborShape_Box27()}
#'
#' @return A list of integers, one vector per parcel
#' @export
neighborParcel2Parcel <- function(parcellation, parcel = NA,
  shape.mat = neighborShape_Box27()){
  
  res <- neighborParcel2Voxel(parcellation, parcel, shape.mat)
  #print(res)
  
  res <- lapply(res, .convertIdx2Parcel, partition = parcellation$partition)
  
  res
}

.convertIdx2Parcel <- function(vec, partition){
  sort(unique(partition[vec]))
}