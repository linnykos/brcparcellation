neighborVoxel2Voxel <- function(parcellation, voxel = NA,
  shape.mat = neighborShape_Box27()){
  if(class(parcellation) != "BrcParcellation")
    stop("parcellation must be of class BrcParcellation")
  if(!brcbase::isValid(parcellation))
    stop("parcellation must be a valid BrcParcellation")
  
  if(all(is.na(voxel))){
    grid <- .formGrid(parcellation$dim3d)
    voxel <- 1:prod(parcellation$dim3d)
    
  } else {
    .is.nonNegInteger(voxel, "voxel")
    if(any(voxel > prod(parcellation$dim3d))) stop(paste("voxel cannot",
      "contain values larger than prod(parcellation$dim3d)"))
    
    grid <- t(sapply(voxel, brcbase::voxelIdxTo3D, dim3d = parcellation$dim3d)) 
  }
  
  grid <- .splitIntoRows(grid)
  func <- .neighborShapeClosure(parcellation$dim3d, shape.mat)
  lis <- lapply(grid, func)
  
  names(lis) <- as.character(voxel)
  
  lis
}

#code to split a matrix into a list of its rows from
# http://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r
# Thanks google!
.splitIntoRows <- function(mat){
  lapply(seq_len(nrow(mat)), function(i) as.numeric(mat[i,]))
}

.formGrid <- function(dim3d){
  dim1 <- 1:dim3d[1]; dim2 <- 1:dim3d[2]; dim3 <- 1:dim3d[3]
  
  expand.grid(dim1, dim2, dim3)
}

.findNeighborSingleParcel <- function(idx, parcellation){
  stopifnot(is.numeric(idx), idx %in% unique(parcellation$partition),
    length(idx) == 1)
  stopifnot(class(parcellation) == "BrcParcellation")
  
}

.neighborShapeClosure <- function(dim3d, shape.mat){
  stopifnot(is.matrix(shape.mat), ncol(shape.mat) == 3)
  stopifnot(length(dim3d) == 3)
  
  func <- function(vec){
    stopifnot(length(vec) == 3)
    
    mat <- apply(shape.mat, 1, function(x){x+vec})
    mat[1,][mat[1,] < 1] <- 1
    mat[1,][mat[1,] > dim3d[1]] <- dim3d[1]
    mat[2,][mat[2,] < 1] <- 1
    mat[2,][mat[2,] > dim3d[2]] <- dim3d[2]
    mat[3,][mat[3,] < 1] <- 1
    mat[3,][mat[3,] > dim3d[3]] <- dim3d[3]

    idx.vec <- apply(mat, 2, brcbase::voxel3DToIdx, dim3d = dim3d)
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