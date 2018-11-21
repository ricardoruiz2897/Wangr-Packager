#'Unnest Single Column
#'
#'@description
#'unnest.single.col takes a single column where each element contains either Null,
#'a tibble with a single column and single row, or a tibble with zero rows and then
#'it returns a single list where both Nulls and tibbles with zero rows return NA otherwise
#'the value is the single value in the tibble.
#'
#'@Usage
#'unnest.single.col(column)
#'
#'@param column a single column where each element contains either Null,
#'a tibble with a single column and single row, or a tibble with zero rows
#'
#'@return a single list where both Nulls and tibbles with zero rows return NA otherwise
#'the value is the single value in the tibble
#'
#'@example
#'
#'column <- list(tibble(a=list(5)), NULL, tibble())
#'
#'result <- unnest.single.col(column)
#'
#'print(result)
#'output: [5, NA, NA]
#'
unnest.single.col <- function(column){

  # Create a list for the results.
  result <- list()

  # For each element in colum.
  for(i in 1:length(column)){

    # Get the next element in the column.
    element <- column[[i]]

    # If the element is null:
    if(is_null(nrow(element))){
      result[[i]] <- NA
    }

    # Else if the element is a zero row tibble:
    else if(nrow(element)==0){
      result[[i]] <- NA
    }

    # Else the element is a single row single column tibble.
    else{
      result[[i]] <- element[1, 1]
    }
  }

  return(result)
}
