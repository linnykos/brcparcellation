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