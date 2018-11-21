#'TABE Grade Lvl
#'
#'@description
#'This aggregates TABE grade equivalents to grade levels
#'
#'@Usage
#'tabe.grade.lvl(df, col.names)
#'@param df data frame
#'@param col.names column names of TABE grade equivalents
#'@return
#'the return is one new column for every column listed in col.names, with .lvl added to the original col.name
#'@examples
#'
# df is the data frame.
# col.names is a list of all the column names to modify in this way.
tabe.grade.lvl <- function(df, col.names){

  # For each column name in col.names
  for(col.name in col.names){

    # Create the new column name by adding .lvl at the end of col.name
    new.col.name <- paste(col.name, "lvl", sep=".")

    df <- df %>%
      mutate(!!new.col.name := case_when(df[[col.name]] < 6 ~ "EL",
                                         df[[col.name]] >= 6 & df[[col.name]] < 9 ~ "MS",
                                         df[[col.name]] >= 9 ~ "HS"))

    df[[new.col.name]] <- factor(df[[new.col.name]],
                                 levels = c("EL", "MS", "HS"),
                                 ordered = T)
  }

  return(df)
}
