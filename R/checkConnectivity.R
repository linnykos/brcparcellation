checkConnectivity <- function(parcellation, parcel = NA,
  shape.mat = neighborShape_Box27()){
   .is.BrcParcellation(parcellation)
  parcel <- .check.parcel(parcel, parcellation$partition)
  if(any(parcel == 0)) parcel <- parcel[parcel != 0]
  
  voxel <- which(parcellation$partition %in% parcel)
  parcel.mapping <- parcellation$partition[voxel]
  res <- neighborVoxel2Voxel(parcellation, voxel = voxel, 
    shape.mat = shape.mat)
  
  connected.bool <- sapply(1:length(res), function(x){
    if(length(which(parcellation$partition[res[[x]]] == parcel.mapping[x])) <= 1) { 
      return(FALSE) 
    } else
      return(TRUE)
  })
  
  connected <- sapply(parcel, function(x){
    idx <- which(parcel.mapping == x)
    if(length(idx) == 1) return(TRUE)
    all(connected.bool[idx]) 
  })
  names(connected) <- parcel
  
  connected
}