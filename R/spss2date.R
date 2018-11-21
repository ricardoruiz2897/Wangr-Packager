#'Spss to date
#'
#'@description
#'This function must be used if the date values are stored are SPSS "dates",
# which are numeric values which represent the number of seconds
# from the beginning of the Gregorian calendar: Oct. 14, 1582.
#'
#'@usage
#'spss2date(.col)
#'
#'@param col Dataframe to change the date format from spss.
#'
#'@return
#'A column with dates in R format.
#'
#'@examples
#'#Let dates.in.spss be a column with spss dates
#'dates <- spss2date(dates.in.spss)
#'
#'
# spss2date ---------------------------------------------------------------
spss2date <- function(col) as.Date(col/86400, origin = "1582-10-14")
# must use the spss2date function if the values stored are SPSS "dates",
# which are numeric values which represent the number of seconds
# from the beginning of the Gregorian calendar: Oct. 14, 1582
