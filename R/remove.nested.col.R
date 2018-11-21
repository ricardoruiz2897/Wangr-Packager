#'Remove Nested Column
#'
#'@description
#'This funciton removes a column from a tibble which is within a nested column of another tibble.
#'
#'@Usage
#'remove.nested.col(nested.col, col.to.rm)
#'
#'@param nested.col the column that is nested
#'
#'@param col.to.rm the column within the nested column to be removed. (Name of column with quotes.)
#'
#'@return the nested column without the column that was supposed to be removed
#'
#'@examples
#'data <- a data frame with a nested column called nest, the nested column has two columns, a and b.
#'
#'result <- remove.nested.col(data$nest, "a")
#'
#'result will the column data$nest without the column a.
#'

# Removes columns from nested tibbles.
remove.nested.col <- function(df, nested.col, col.to.rm){

  #Make this column name as string.
  nested.col <- deparse(substitute(nested.col))
  col.to.rm <- deparse(substitute(col.to.rm))

  #Convert to a list
  temp <- nested.col
  nested.col <- df[nested.col][[1]]

  #For each tibble in nested, remove col.to.rm
  tmp <- function(tibble, col.to.rm){

    tibble <- select(tibble, -one_of(col.to.rm))
    return(tibble)

  }

  to.return <- purrr::map(nested.col, tmp, col.to.rm)
  df[temp][[1]] <- to.return

  return(df)

}

