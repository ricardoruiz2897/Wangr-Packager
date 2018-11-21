#'Change NA
#'@description
#'This changes NA values; the default is to change NA's to 0
#'
#'@Usage
#'change.na(column, change.to = 0)
#'@param column column name to change NAs
#'@param change.to value to change na to (default is 0)
#'@return
#'the return value is what NA is changed to
#'@examples
#'df$col <- change.na(df$col, change.to = 0)
#'df$col <- change.na(df$col, change.to = -999)
#'
change.na <- function(df, column, change.to = 0){

  was.factor <- FALSE

  temp <- deparse(substitute(column))
  column <- df[temp]

  if(is.factor(column)){

    column <- as.character(column)
    was.factor <- TRUE

  }

  column[is.na(column)] <- change.to

  column <- replace(column, sapply(column, is.null), change.to)

  if(was.factor){

    column <- as.factor(column)

  }

  df[temp] <- column

  return(df)
}
