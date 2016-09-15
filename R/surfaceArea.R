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
