#' Find neighboring voxels for each voxel
#' 
#' Given a \code{BrcParcellation} object and a vector of voxels
#' indices in \code{voxel}, find all the voxels that neighbor each of
#' the voxels. By default, \code{voxel} is set to \code{NA}, meaning
#' it finds the neighbor of all voxels.
#' 
#' The \code{voxel} vector are indicies referring of specific locations in
#' \code{parcellation}. They must be positive integers between \code{1} and 
#' \code{length(parcellation$partition)}.
#'
#' @param parcellation a \code{BrcParcellation} object
#' @param voxel a vector of indicies of which voxels to select the neighbors of
#' @param shape.mat the output of \code{neighborShape} function, such as \code{neighborShape_Box27()}
#'
#' @return A list of integers, one vector per voxel
#' @export
neighborVoxel2Voxel <- function(parcellation, voxel = NA,
  shape.mat = neighborShape_Box27()){
  .is.BrcParcellation(parcellation)
  
  grid <- .check.voxel(voxel, parcellation$dim3d)
  if(all(is.na(voxel))) voxel <- 1:prod(parcellation$dim3d)
  
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

#' Neighbor shape for box
#' 
#' This is a function needed for many functions such as 
#' \code{neighborVoxel2Voxel} to determine how neighbors are defined.
#' This will typically be the \code{shape} argument to many such functions.
#' 
#' This shape is defined in such a way that if the voxel in question is
#' the middle voxel of a 3x3x3 cube, all the voxels consisting of the 3x3x3
#' cube are the neighboring voxels.
#'
#' @return a matrix with 3 columns
#' @export
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

#' Neighbor shape for diamond
#' 
#' This is a function needed for many functions such as 
#' \code{neighborVoxel2Voxel} to determine how neighbors are defined.
#' This will typically be the \code{shape} argument to many such functions.
#' 
#' This shape is defined in such a way that if the voxel in question is
#' a 6-sided die, the neighboring voxels are the 6 voxels immediately bordering
#' each face of the die.
#'
#' @return a matrix with 3 columns
#' @export
neighborShape_Diamond7 <- function(){
  matrix(c(0,0,0,
           0,0,1, 0,0,-1,
           0,1,0, 0,-1,0,
           1,0,0, -1,0,0), ncol = 3, nrow = 7, byrow = T)
}