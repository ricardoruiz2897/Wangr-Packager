#'Change in Wages
#'
#'@description
#'This function takes in two numbers x and y which represent wages and determines
#'the difference of y - x. If x or y is NA then it is changed to zero.
#'
#'@Usage
#'change.in.wages(x, y)
#'
#'@param x the initial wage or NA
#'
#'@param y the ending wage or NA
#'
#'@return
#'This function returns y-x
#'
#'@examples
#'answer <- change.in.wages(5, 10)
#'print(answer)
#'
#'output: 5
#'
#'answer <- change.in.wages(5, NA)
#'print(answer)
#'
#'output: -5
#'
change.in.wages <- function(x, y){

  # if x or y is bad input, then return NULL.
  if(is_null(x) || is_null(y)){
    return(NULL)
  }

  if(is.na(x)){
    x <- 0
  }
  if(is.na(y)){
    y <- 0
  }
  return(y - x)
}
