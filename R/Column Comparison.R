comp.cols <- function(dat, col1, col2){

  if(class(col1) != class(col2)){
    print(paste("WARNING: Columns are not the same class. Col1 is class",class(col1),
             "and Col2 is class", class(col2)))
    return(dat)
  }

  if(length(col1) != length(col2)){
    print("WARNING: column lengths are not equal")
    return(dat)
  }

  if(class(col1) == "character"){
    col1 <- trimws(col1, which = "both")
    col2 <- trimws(col2, which = "both")
  }

  dat$logic.comp <- ifelse(is.na(col1) == T & is.na(col2) == T, TRUE,
                           ifelse(is.na(col1) == T & is.na(col2) == F, FALSE,
                                  ifelse(is.na(col1) == F & is.na(col2) == T, FALSE,
                                         ifelse(col1 == col2, TRUE, FALSE))))

  return(dat)
}



# # testing #
#
# # date
#
# test <- tibble(date1 = as.Date(c("2017-03-30", "2017-08-31", NA, NA), format = "%Y-%m-%d"),
#                date2 = as.Date(c("2017-03-30", "2017-08-30", NA, "2016-12-08"), format = "%Y-%m-%d"))
#
# out <- logic.comp(test, test$date1, test$date2)
# out
#
#
# # incompatible types
#
# test$date1 <- as.character(test$date1)
#
# out <- logic.comp(test, test$date1, test$date2)
# out
#
#
# # numeric
#
# test <- tibble(num1 = c(1,2,NA,NA),
#                num2 = c(1,1,NA,1))
#
# out <- logic.comp(test, test$num1, test$num2)
# out
#
#
# # strings
#
# test <- tibble(str1 = as.character(c("test","test",NA,NA,"mess")),
#                str2 = as.character(c("test","mess",NA,"test"," mess")))
#
# out <- logic.comp(test, test$str1, test$str2)
# out
#
#
#





