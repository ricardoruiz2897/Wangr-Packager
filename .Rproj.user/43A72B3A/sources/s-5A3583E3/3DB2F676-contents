#Logical to Numeric

#take given column filled with logical T/F statements and returns 0 for false and 1 for true
change.to.numeric <- function(col){

  change.tmp <- function(element){
    if(element == TRUE){
      return(1)
    }
    else if(element == FALSE){
      return(0)
    }
    else {
      return(NA)
    }
  }
  results <- purrr::map(col, change.tmp)
  return(unlist(results))
}
