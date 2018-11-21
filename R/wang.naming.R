#'Wang Naming
#'
#'@description
#'Change names in dataframe, to Dr. Wang's naming convention.
#'
#'@usage
#'wang.naming(.df)
#'
#'@param df Dataframe to change the column names
#'
#'@return
#'Dataframe with names changed to Dr. Wang's naming convention.
#'
#'@examples
#'#Returns dataframe with names changed.
#'df <- wang.naming(df)
#'
# wang.naming -------------------------------------------------------------
wang.naming <- function(df) {

  if(!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
  }

  names(df) <- tolower(names(df))
  names(df) <- gsub("_", ".", names(df))
  names(df) <- gsub(" ", ".", names(df))
  names(df) <- gsub("/", ".", names(df))
  names(df) <- gsub("-", ".", names(df))
  names(df) <- gsub(",", ".", names(df))
#  names(df) <- gsub("..", ".", names(df))

  return(df)

}

