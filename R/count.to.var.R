#' count.to.var
#'
#' @description
#' This function takes a nested factor column and creates new (unnested) variables with counts of the factor levels
#'
#' @Usage
#' count.to.var(df, col, nested.factor, var.name.list, factor.level.list, na2=0)
#'
#' @param df dataframe name
#' @param col String name of nested column. (With quotes."")
#' @param nested.factor name of nested factor within the nested column
#' @param var.names.list vector of new variable names
#' @param factor.levels.list vector of factor levels to extract
#' @param na2 replace na with; default is 0
#'
#' @return
#' A dataframe with new variables with counts of those
#' @export
#'
#' @examples
#' tmp2 <- count.to.var(df = tmp,
#'                   col = "pre",
#'                   nested.factor = "level",
#'                    var.names.list = var.names.list,
#'                    factor.levels.list = factor.levels.list,
#'                    na2 = 0)
count.to.var <- function(df, col, nested.factor,
                         var.names.list,
                         factor.levels.list,
                         na2 = 0){

  if(!require("tidyverse")){
    install.packages("tidyverse")
    require("tidyverse")
  }

  if(!require("lazyeval")){
    install.packages("lazyeval")
    require("lazyeval")
  }

  #Lists not same length
  if(length(var.names.list) != length(factor.levels.list)){
    print("Error: Lists should be the same length.")
    return(df)
  }

  #Col is not character
  if(typeof(col) != "character"){
    print("Error: Col should be of type character.")
    return(df)
  }

  #Nested.factor is not character.
  if(typeof(nested.factor) != "character"){
    print("Error: nested.factor should be of type character.")
    return(df)
  }

  for(i in 1:length(var.names.list)){

    #Create new list with filtered values.
    first.map.result <- map_if(df[,col][[1]], ~!is.null(.x), #The one here is not part of the iteration.
                               ~filter_(.x, interp(~v==factor.levels.list[i], v=as.name(nested.factor))))
    #prepare new column and change it's name to the one on the list
    df$new <- map(first.map.result, nrow)
    names(df)[names(df) == 'new'] <- var.names.list[i]

  }


  df[var.names.list] <- unlist(map(df[var.names.list], replace_na, 0))

  return(df)

}

# Example
# var.names.list <- c("pre.fel.arrests", "pre.mis.arrests")
# factor.levels.list <- c("Felony", "Misdemeanor")
# tmp2 <- count.to.var(df = tmp,
#                    col = "pre",
#                    nested.factor = "level",
#                    var.names.list = var.names.list,
#                    factor.levels.list = factor.levels.list,
#                    na2 = 0
# )
