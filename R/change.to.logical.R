#'Change to Logical
#'
#'@description
#'This function takes a column and changes a true or false representation to the R Language logical(boolean) native representation.
#'
#'@Usage
#'change.to.logical(.col)
#'
#'@param df Dataframe that contains the column to change to logical.
#'
#'@param col Column in df to change to logical
#'
#'@param is_colum specified is the return value is a column. Use if implementing map function.
#'
#'@param from_list specied if using map funciton and going through a list of strings in which each string has a column name.
#'
#'@return
#'A df with a column/vector with R logical representations.
#'
#'@examples
#'column1 <- c(0, 1, 1, 1, 0)
#'column2 <- c("F", "F", "T", "F")
#'column3 <- c("no", "no", "no", "yes", "no")
#'
#'df$column1 <- column1
#'df$column1 <- column1
#'df$column1 <- column1
#'
#'column1 <- change.to.logical(df, column1)
#'column2 <- change.to.logical(df, column2)
#'column3 <- change to.logical(df, column3)
#'
#We use this to convert column to logical values
change.to.logical <- function(df, col, from_list = F, is_column = F){

  #Change to string for easy manipulation
  if(!from_list){
    col <- deparse(substitute(col))
  }

  temp <- df[[col]]

  #Create lists of possible values which can mean either true or false
  false.list <- c(0, "FALSE", "F", "f", "False", "false", FALSE, "no", "No", "N")
  true.list <- c(1, "TRUE", "T", "t", "True", "true", TRUE, "yes", "Yes", "Y")

  Na <- c(NA)

  change.tmp <- function(element){

    #if is in the true list set by index to true
    if(element %in% true.list){

      return(TRUE)

    }
    #if is in the false list set by index to false
    else if(element %in% false.list){

      return(FALSE)


    }
    else if(element %in% Na){

      return(NA)

    }

    #if it is not, then is unknown and we set it to NA
    else {

      return(NA)

    }

  }

  #purrr::map(temp, print)
  results <- purrr::map(temp, change.tmp)
  results <- unlist(results)

  df[[col]] <- results

  if(is_column){
    return(results)
  }else{
    return(df)
  }

}

