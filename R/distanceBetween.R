#' Compute the distance between parcels in a parcellation
#' 
#' This function computes the distance between parcels in a given parcellation.
#' 
#' The distance between two parcels is passed into this function as another function,
#' such as \code{distance_manhattan} for example, as the \code{dist} argument.
#' The \code{dist} function must be able to take two matrices, each with 
#' 3 rows, denoting the 3-dimensional indices (locations) of all the voxels in
#' each of the two parcels. 
#'
#' @param parcellation a \code{BrcParcellation} object
#' @param dist a distance function
#' @param ... additional arguments to be passed into \code{dist} (optional)
#'
#' @return a \code{dist} object
#' @export
distanceBetween <- function(parcellation, dist = distance_manhattan, ...){
  .is.BrcParcellation(parcellation)
  
  numPar <- brcbase::numParcels(parcellation)
  if(numPar == 1) {return(stats::as.dist(0))}
  
  mat <- matrix(0, numPar, numPar)
  
  for(i in 2:numPar){
    for(j in 1:(i-1)){
      mat[i,j] <- .compute_parcellationDistance(parcellation, i, j, dist, ...)
    }
  }
  
  stats::as.dist(mat)
}

#' A distance function between to matrices (Manhattan distance)
#' 
#' The input to this function are two matrices with the same number of rows.
#' This function computes the minimum
#' distance between any column in \code{mat1} to any column in \code{mat2}.
#'
#' @param mat1 a matrix
#' @param mat2 a matrix
#'
#' @return a numeric
#' @export
distance_manhattan <- function(mat1, mat2){
  n1 <- ncol(mat1)
  n2 <- ncol(mat2)
  
  if(n1 < n2){
    mat.small <- mat1; mat.big <- mat2
  } else {
    mat.small <- mat2; mat.big <- mat1
  }
  
  dis <- matrix(0, nrow = ncol(mat.big), ncol = ncol(mat.small))
  for(i in 1:ncol(dis)){
    dis[,i] <- colSums(abs(mat.small[,i] - mat.big))
  }
  
  dis
}

.compute_parcellationDistance <- function(parcellation, i, j, dist, ...){
  mat1 <- sapply(which(parcellation$partition == i), brcbase::voxelIdxTo3D,
    dim3d = parcellation$dim3d)
  mat2 <- sapply(which(parcellation$partition == j), brcbase::voxelIdxTo3D,
    dim3d = parcellation$dim3d)
  
  dis <- dist(mat1, mat2, ...)
  min(dis)
}