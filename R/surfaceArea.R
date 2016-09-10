surfaceArea <- function(parcellation, parcel = NA,
  shape.mat = neighborShape_Box27()){
   .is.BrcParcellation(parcellation)
  parcel <- .check.parcel(parcel, parcellation$partition)
  if(any(parcel == 0)) parcel <- parcel[parcel != 0]
 
  voxel <- which(parcellation$partition %in% parcel)
  parcel.mapping <- parcellation$partition[voxel]
  res <- neighborVoxel2Voxel(parcellation, voxel = voxel, 
    shape.mat = shape.mat)
  
  on.boundary <- sapply(res, function(x){
    if(length(unique(parcellation$partition[x])) > 1) { return(TRUE) } else
      return(FALSE)
  })
  
  surface.vec <- sapply(parcel, function(x){
    sum(on.boundary[which(parcel.mapping == x)])
  })
  names(surface.vec) <- parcel
  
  surface.vec
}

