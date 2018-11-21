#'Change to NA
#'
#'@description
#'This changes values to NA; the default is to change 0's to NAs
#'
#'@Usage
#'change.to.na(column, change = 0)
#'@param column column name to change values to NAs
#'@param change value to change to NA (default is 0)
#'@return
#'the return value is the user-defined value
#'@examples
#' v1 <- c(0, 1, 0)
#' v1 <- as.factor(v1)
#' v2 <- change.to.na(v1, change = 0)
#' df$col <- change.to.na(df$col, change = -999)

change.to.na <- function(df, column, change = 0){

  column <- deparse(substitute(column))

  df[column] <- na_if(df[column], 0)

  return(df)

}

### Wang 2018-08-29:  can we insert dplyr::na_if?

