findNeighborVoxels <- function(parcellation, include.boundary = F){
  if(class(parcellation) != "BrcParcellation")
    stop("parcellation must be of class BrcParcellation")
  if(!brcbase::isValid(parcellation))
    stop("parcellation must be a valid BrcParcellation")
  
}

findNeighborParcels <- function(parcellation, include.boundary = F){
  if(class(parcellation) != "BrcParcellation")
    stop("parcellation must be of class BrcParcellation")
  if(!brcbase::isValid(parcellation))
    stop("parcellation must be a valid BrcParcellation")
  
  
}

.findNeighborSingleParcel <- function(idx, parcellation){
  stopifnot(is.numeric(idx), idx %in% unique(parcellation$partition),
    length(idx) == 1)
  stopifnot(class(parcellation) == "BrcParcellation")
  
  vec <- brcbase::convertVoxelIndexto3D(parcellation$dim3d, idx)
  
}

.neighborShapeClosure <- function(dim3d, shape.mat){
  stopifnot(is.matrix(shape.mat), ncol(shape.mat) == 3)
  stopifnot(length(dim3d) == 3)
  
  func <- function(vec){
    stopifnot(length(vec) == 3)
    
    mat <- apply(shape.mat, 1, function(x){x+vec})
    mat[,1][mat[,1] < 1] <- 1
    mat[,1][mat[,1] > dim3d[1]] <- dim3d[1]
    mat[,2][mat[,2] < 1] <- 1
    mat[,2][mat[,2] > dim3d[2]] <- dim3d[2]
    mat[,3][mat[,3] < 1] <- 1
    mat[,3][mat[,3] > dim3d[3]] <- dim3d[3]
    
    idx.vec <- apply(mat, 2, brcbase::convertVoxel3DtoIndex, dim3d = dim3d)
    unique(sort(idx.vec))
  }
  
  func
}

neighborShape_Box27 <- function(){
  matrix(c(0,0,0,
           0,0,1, 0,0,-1,
           0,1,0, 0,-1,0,
           1,0,0, -1,0,0,
           0,1,1, 0,-1,-1,
           1,0,1, -1,0,-1,
           1,1,0, -1,-1,0,
           1,-1,0, -1,1,0,
           1,0,-1, -1,0,1,
           0,1,-1, 0,-1,1,
           1,1,1, -1,-1,-1,
           1,1,-1, -1,-1,1,
           1,-1,1,  -1,1,-1,
           -1,1,1,  1,-1,-1), ncol = 3, nrow = 27, byrow = T)
}

neighborShape_Diamond7 <- function(){
  matrix(c(0,0,0,
           0,0,1, 0,0,-1,
           0,1,0, 0,-1,0,
           1,0,0, -1,0,0), ncol = 3, nrow = 7, byrow = T)
}