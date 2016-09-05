neighborVoxel2Parcel <- function(parcellation, voxel = NA,
  shape.mat = neighborShape_Box27()){
  
  res <- neighborVoxel2Voxel(parcellation, voxel, shape.mat)
  res <- lapply(res, .convertIdx2Parcel, partition = parcellation$partition)
  
  res
}

neighborParcel2Voxel <- function(parcellation, parcel = NA,
  shape.mat = neighborShape_Box27()){
  
}

neighborParcel2Parcel <- function(parcellation, parcel = NA,
  shape.mat = neighborShape_Box27()){
  
}

.convertIdx2Parcel <- function(vec, partition){
  sort(unique(partition[vec]))
}