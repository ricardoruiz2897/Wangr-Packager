#'Change to vector string.
#'
#'@description
#'Returns a vector of strings(character vector) with the name of the columns in vector.
#'
#'@Usage
#'change.to.vect.str(df, vector)
#'
#'@param df A dataframe with the columns we want to put in a vector string.
#'@param vector Numeric vector with the number of columns from the dataframe we want to make a vector string.
#'
#'@return
#'A vector of strings with the names of the columns from df.
#'
#'@examples
#'df <- my_dataframe
#'
#'string.vectors <- change.to.vect.str(df, c(1:20))
#'

#This function returns a vector of strings with the name of columns.
change.to.vect.str <- function(df, vector){

  str.vector <- c()

  for(i in vector){
    str.vector <- c(str.vector,colnames(df)[i])
  }

  return(str.vector)

}
