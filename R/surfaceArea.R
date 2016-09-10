surfaceArea <- function(parcellation, parcel = NA,
  shape = neighborShape_Box27()){
  if(class(parcellation) != "BrcParcellation")
    stop("parcellation must be of class BrcParcellation")
  if(!brcbase::isValid(parcellation))
    stop("parcellation must be a valid BrcParcellation")
  
  if(all(is.na(parcel))){
    parcel <- sort(unique(parcellation$partition))
  } else {
    .is.nonNegInteger(parcel, "parcel")
    if(any(parcel > max(parcellation$partition))) stop(paste("parcel cannot",
      "contain values larger than max(parcellation$partition)"))
  }
  
  
}