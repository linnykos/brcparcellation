neighborVoxel2Parcel <- function(parcellation, voxel = NA,
  shape.mat = neighborShape_Box27()){
  
  res <- neighborVoxel2Voxel(parcellation, voxel, shape.mat)
  res <- lapply(res, .convertIdx2Parcel, partition = parcellation$partition)
  
  res
}

neighborParcel2Voxel <- function(parcellation, parcel = NA,
  shape.mat = neighborShape_Box27()){
  
  if(all(is.na(parcel))){
    parcel <- sort(unique(parcellation$partition))
  } else {
    .is.nonNegInteger(parcel, "parcel")
    if(any(parcel > max(parcellation$partition))) stop(paste("parcel cannot",
      "contain values larger than max(parcellation$partition)"))
  }
  
  res <- lapply(1:length(parcel), function(x){
    idx <- which(parcellation$partition == x)
    voxelNeigh <- neighborVoxel2Voxel(parcellation, voxel = idx,
      shape.mat)
    sort(unique(unlist(voxelNeigh)))
  })
  
  names(res) <- parcel
  res
}

neighborParcel2Parcel <- function(parcellation, parcel = NA,
  shape.mat = neighborShape_Box27()){
  
  res <- neighborParcel2Voxel(parcellation, parcel, shape.mat)
  res <- lapply(res, .convertIdx2Parcel, partition = parcellation$partition)
  
  res
}

.convertIdx2Parcel <- function(vec, partition){
  sort(unique(partition[vec]))
}