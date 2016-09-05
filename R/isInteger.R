.is.nonNegInteger <- function(vec, name){
  if(!all(!is.na(vec))) stop(paste(name, 
    "cannot contain NA and non-NA elements"))
  if(!is.numeric(vec)) stop(paste(name, "must be a numeric vector"))
  if(any(vec %% 1 != 0)) stop(paste(name, "must be a vector of integers"))
  if(any(vec < 0)) stop(paste(name, "must be a vector of positive integers"))
  if(any(duplicated(vec))) stop(paste(name, "must contain all unique",
    "elements"))
  
  TRUE
}