#'Fix min max
#'
#'@description
#'Changes all values in column that are less than min to the value in
# change.min.to, and all the values that are greater than max to the value in change.max.to.
#'
#'@usage
#'fix.min.max(.column, .min, .max, change.min.to, change.max.to)
#'
#'@param column A list where each element is of an arbitrary type called A of is NA
#'@param min A value of the same arbitrary type A as in column where all values in column that are less than it, will be changed.
#'NOTE: If min is equal to NA then no element in column will be considered to be less than min.
#'Default Value: NA
#'@param max A value of the same arbitrary type A as in column where all values in column that
#'are greater than it will be changed.
#'NOTE: If max is equal to NA then no element in column will be considered to be greater than max.
#'Default Value: NA
#'@param change.min.to A value of the same arbitrary type A as in column where all values in column that are less than min will be changed to the value stored in change.min.to.
#'Default Value: NA
#'@param change.max.to A value of the same arbitrary type A as in column where all values in
#'column that are greater than max will be changed to the value stored in change.max.to.
#'Default Value: NA
#'
#'@return
#'Column with fixed min and max.
#'
#'@examples
#'See vignette for example.
#'
fix.min.max <- function(column, min = NA, max = NA, change.min.to = NA, change.max.to = NA){

  # Get the class type of the data stored in column.
  type <- class(column[[1]])

  # If min is not NA, change all values below min in column to change.min.to.
  if(!is.na(min)){
    column <-  ifelse(column < min, change.min.to, column)
  }

  # If max is not NA, change all values above max in column to change.max.to.
  if(!is.na(max)){
    column <-  ifelse(column > max, change.max.to, column)
  }

  # If the type of data in the list before was a date, change it to a date.
  if(type == "Date"){
    column <-  as.Date(column, origin="1970-01-01")
  }

  return(column)

}

