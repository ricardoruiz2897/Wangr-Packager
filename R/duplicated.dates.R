#'Duplicated Dates.
#'
#'@description
#'This function will put trues and falses regarding whether a date is withing a range or not.
#'If the date is within a range, it will also check if the row has the same values for each column
#'
#'@Usage
#'duplicated.dates(df, col, days, new.col.name = "nested.logical")
#'
#'@param df A dataframe that has the nested column
#'@param col The nested column.
#'@param days The range of days to check.
#'@param new.col.name The name of the new column in the nested tibbles.
#'
#'@value
#'Returns the column that is nested appending the new column to each tibble.
#'
#'@examples
#'duplicated.dates(csst.nest, "csst", 30)
#'

duplicated.dates <- function(df, col, days, new.col.name = "nested.logical"){

  if(!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
  }


  #Helper functions.

  #Returns true if all the columns are the same except for "csst.date.completed"
  allvarsExcepttDate <- function(my_df, row1, row2){

    for(col in colnames(my_df)){

      if(col != "csst.date.completed"){

        v1 <- my_df[col][[1]][row1]
        v2 <- my_df[col][[1]][row2]

        v1 <- as.character(v1)
        v2 <- as.character(v2)

        if(!is.na(v1==v2) && v1 != v2){
          return(FALSE)
        }

      }

    }

    return(TRUE)

  }


  #Returns the diference of day between two days.
  day_difference <- function(date1, date2){

    diff <- difftime(date1, date2, units = c("days"))

    if(diff < 0){ diff <- diff * -1 }

    return(diff)

  }

  #Returns true if two days are within a days range, false otherwise.
  daysInRange <- function(date1, date2, days){

    if(day_difference(date1, date2) <= days){ return(TRUE) }
    return(FALSE)

  }

  #Select the column to work with.
  df <- df %>%
    select(c(col))

  #Get the column for the one that we are checking.
  tib <- df[col][1]

  #For all of the rows in the main dataframe.
  for(i in 1:nrow(df)){

    logic.vec <- c()

    current_tibble <- tib[[1]][i]

    #Convert to a dataframe for manipulation
    current_tibble <- as.data.frame(current_tibble)

    if(nrow(current_tibble) == 0){

      #do nothing.

    } else if(nrow(current_tibble) == 1){

      logic.vec <- c(logic.vec, FALSE)

    } else {

      #print(as.vector(current_tibble["csst.date.completed"]))

      #First one is always False
      logic.vec <- c(FALSE)

      #logic for comparting dates on current tibble.
      for(j in 1:(length(current_tibble$csst.date.completed)-1)){

        #If in the same date range.
        inRange <- daysInRange(current_tibble$csst.date.completed[j], current_tibble$csst.date.completed[j+1], days)

        #If all rows match except for the date.
        allSameButDate <- allvarsExcepttDate(current_tibble, j, j+1)

        if(inRange){

          if(allSameButDate){

            logic.vec <- c(logic.vec, TRUE)

          } else{

            logic.vec <- c(logic.vec, TRUE)

          }


        } else{

          logic.vec <- c(logic.vec, FALSE)

        }

      }

    }

    current_tibble[new.col.name] <- logic.vec
    #print(as_tibble(current_tibble)$repeated.date)
    df$csst[[i]] <- as_tibble(current_tibble)

  }

  return(df)

}

#df <- my_function(csst.nest, "csst", 30)

