neighborVoxel2Parcel <- function(parcellation, voxel = NA,
  shape.mat = neighborShape_Box27()){
  
  res <- neighborVoxel2Voxel(parcellation, voxel, shape.mat)
  res <- lapply(res, .convertIdx2Parcel, partition = parcellation$partition)
  
  res
}

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