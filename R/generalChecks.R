.is.BrcParcellation <- function(parcellation){
  if(class(parcellation) != "BrcParcellation")
    stop("parcellation must be of class BrcParcellation")
  if(!brcbase::isValid(parcellation))
    stop("parcellation must be a valid BrcParcellation")
  
  TRUE
}

.check.parcel <- function(parcel, partition){
  if(all(is.na(parcel))){
    parcel <- sort(unique(partition))
  } else {
    .is.nonNegInteger(parcel, "parcel")
    if(any(parcel > max(partition))) stop(paste("parcel cannot",
      "contain values larger than max(parcellation$partition)"))
  }
  
  parcel
}

.check.voxel <- function(voxel, dim3d){
  if(all(is.na(voxel))){
    grid <- .formGrid(dim3d)
    
  } else {
    .is.nonNegInteger(voxel, "voxel")
    if(any(voxel > prod(dim3d))) stop(paste("voxel cannot",
      "contain values larger than prod(parcellation$dim3d)"))
    
    grid <- t(sapply(voxel, brcbase::voxelIdxTo3D, dim3d = dim3d)) 
  }
  
  grid
}

.formGrid <- function(dim3d){
  dim1 <- 1:dim3d[1]; dim2 <- 1:dim3d[2]; dim3 <- 1:dim3d[3]
  
  expand.grid(dim1, dim2, dim3)
}