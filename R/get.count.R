#'Get Count
#'
#'@description
#'get.count counts the number of rows with a given entry in the nested column.
#'
#'@Usage
#'get.count(data, "a", "b")
#'
#'@param data the data that contains the column to be counted
#'
#'@param column the column that is nested
#'
#'@param nested.column the column within column that needs to be counted
#'
#'@return a tibble that contains one column called entries which has the thing being counted and
#'count which contains the number of times that the value of entry was found. Note that even if the
#'same value was found multiple times in nested.column for one row in column, it will only count once.
#'
#'@examples
#'
#'data <- tibble(a=list(tibble(b=list(1, 2, 3, 3, 5)), tibble(b=list(1, 6, 3))))
#'
#'result <- get.count(data, "a", "b")
#'
#'print(result)
#'output: |_entry_|_count_|
#'        |   1   |   2   |
#'        |   2   |   1   |
#'        |   3   |   2   |
#'        |   5   |   1   |
#'        |___6___|___1___|
#'
get.count <- function(data, column, nested.column){
  agg_list <- list()

  for(tib in data[[column]]){

    line.entries = list()  # This contains all the unique entries for this row (line).

    for(entry in tib[[nested.column]]){

      entry <- toString(entry)  # Change the entry to a string.

      # If the entry has not been recorded for this row yet.
      if(!(entry %in% line.entries)){

        # If the entry was already in agg_list:
        if(entry %in% names(agg_list)){
          agg_list[[entry]] <- agg_list[[entry]] + 1
        }
        else{
          agg_list[[entry]] <- 1
        }

        # Record the entry currently being observed for the row.
        line.entries[[length(line.entries) + 1]] <- entry
      }

    }
  }

  ### Turn the aggregate list into a tibble.
  agg <- tibble(entry = names(agg_list), count = agg_list)

  agg$count <- unlist(agg$count)

  return(agg)
}
