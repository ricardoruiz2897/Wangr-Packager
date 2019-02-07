#'TABE ef lvl
#'
#'@description
#'This function creates grade-level groupings of the TABE for Windham School District use
#'
#'@Usage
#'tabe.ef.lvl(df, col.names)
#'
#' @param df data frame
#' @param col.names column name(s) of TABE grade equivalents
#'
#' @return
#' the return is one new column for every column listed in col.names, with .lvl added to the original col.name
#' @export
#'
#' @examples
#' df <- df %>% tabe.ef.lvl(df, "read.ge")
#'
# 2018-11-26 Dr. Wang
#     But still not working way I want it to

tabe.ef.lvl <- function(df, col.names){

  # For each column name in col.names
  for(col.name in col.names){

    # Create the new column name by adding .lvl at the end of col.name
    new.col.name <- paste(col.name,"ef", "lvl", sep=".")

    df <- df %>%
      mutate(!!new.col.name := case_when(df[[col.name]] < 2 ~ "1",
                                         df[[col.name]] >= 2 & df[[col.name]] < 4 ~ "2",
                                         df[[col.name]] >= 4 & df[[col.name]] < 6 ~ "3",
                                         df[[col.name]] >= 6 & df[[col.name]] < 9 ~ "4",
                                         df[[col.name]] >= 9 & df[[col.name]] < 11 ~ "5",
                                         df[[col.name]] >= 11 ~ "6"))

    #    df[[new.col.name]] <- factor(df[[new.col.name]],
    #                                 levels = c("1", "2", "3", "4", "5", "6"),
    #                                 ordered = T)
  }

  return(df)
}
