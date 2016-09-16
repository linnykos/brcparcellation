createMask <- function(parcellation){
  .is.BrcParcellation(parcellation)
  
  parcellation$partition[parcellation$partition != 0] <- 1
  
  parcellation
}