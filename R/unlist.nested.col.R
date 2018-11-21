#Unlist nested column.
unlist.nested.col <- function(df, nested.col, replace.null.with = NA){

  #We need dplyr for piping and filter_
  if(!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
  }

  #Check for missing arguments
  if(missing(df)){
    print("Data is missing.")
    return()
  }

  if(missing(nested.col)){
    print("Nested column is missing.")
    return()
  }

  #Get the column we want.
  temp <- df %>%
    select(nested.col)

  #Replace, unlist
  temp[[1]] <- replace(temp[[1]], sapply(temp[[1]], is.null), replace.null.with) %>%
    unlist(temp[[1]])



  return(temp[[1]])

}
