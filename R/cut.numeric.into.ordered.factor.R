#'Cut Numeric into Ordered Factor
#'
#'@description
#'This function takes a df, a column name, a new column name, and a number, and returns a df with a new column with the given column cut into an ordered factor.
#'
#'@Usage
#'cut.numeric.into.ordered.factor(df, col, new.col, n.levels)
#'
#'@param df Dataframe to take info from.
#'@param col Column with the information to cut.
#'@param new.col Name of the new column.
#'@param n.levels Number of levels to cut.
#'
#'@return Returns a df with the new column.

#'@examples
#'df <- cut.numeric.into.ordered.factor(df, "population", "levels", 5)

cut.numeric.into.ordered.factor <- function(df, col, new.col, n.levels) {

  make.label <- function(isTop = FALSE, isButtom = FALSE, c) {

    if(isTop == T){

      low <- as.character(c[[1]][1])
      s <- paste(">= ", low, sep = " ")

      return(s)

    } else if(isButtom == T){

      high <- as.character(c[[1]][2])
      s <- paste("<=", high, sep = "")

      return(s)

    } else {

      low <- as.character(c[[1]][1])
      high <- as.character(c[[1]][2])

      s <- paste(low, high, sep = "-")

      return(s)

    }

  }

  #Returns True if a in range of the tuple c
  inRange <- function(a, c){
    s <- c[[1]][1]
    e <- c[[1]][2]

    if(a <= e && a >= s){
      return(TRUE)
    } else {
      return(FALSE)
    }

  }

  vec <- df[[col]]

  #Find the max number for the column.
  maximum <- max(vec)
  minimum <- min(vec)

  #Find the number that will be the relative difference between levels.
  if(minimum <= 0){

    rangeLevel <- (maximum+minimum)/n.levels

  } else{

    rangeLevel <- (maximum-minimum)/n.levels

  }


  #Make list of ranges
  ranges <- list()

  current <- 1
  currentLess <- minimum
  for(i in 1:n.levels){

    ranges[[current]] <- c(currentLess, currentLess+rangeLevel)
    current <- current + 1
    currentLess <- currentLess + rangeLevel

  }

  result.vector <- c()
  #Put all into their level
  for(i in 1:length(df[[col]])){

    current <- df[[col]][i]
    falls <- FALSE

    for(j in 1:length(ranges)){

      if(inRange(current, ranges[j])){

        falls <- TRUE

        if(inRange(current, ranges[j])){

          if(j == 1){

            r <- make.label(isButtom = T, c =  ranges[j])

          } else if (j == length(ranges)){

            r <- make.label(isTop = T, c = ranges[j])

          } else {

            r <- make.label(c = ranges[j])

          }

        }

        result.vector <- c(result.vector, r)

      }

    }

    if(falls == FALSE){
      print(current)
      print(ranges)
    }

  }

  df[[new.col]] <- result.vector
  df[[new.col]] <- ordered(df[[new.col]])
  return(df)

}






