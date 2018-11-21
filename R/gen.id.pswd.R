#'Generate Id-Passwords
#'
#'@description
#'This function appends two columns to a given dataframe.
#'One column will be of random numbers and the other column of random strings that include numbers and letters.
#'
#'@Usage
#'gen.id.pswd(.df, str.len)
#'
#'@param df A dataframe to put the two new columns
#'@param str.len The length of each password string in string.id column. Defaults to 10.
#'
#'@return
#'A dataframe with two new columns, number.id and string.id.
#'
#'@examples
#'df <- my.dataframe
#'
#'df <- get.id.pswd(df)
#'df <- get.id.pswd(df=df, str.len=7)
#'
#'


#Create number id and passwords.
gen.id.pswd <- function(df, str.len=10){

  if(!require("random")){
    install.packages("random")
    require("random")
  }

  size <- nrow(df)

  #Generate random number
  base.number <- sample(100000:400000, 1)

  #Create number id column.
  #Get size value.
  number.ids <- c(base.number:((base.number+size)-1))
  df$number.id <- as.double(number.ids)

  #Create random id passwords
  passwords <- randomStrings(n=size, len = str.len, digits=T)
  passwords <- as.vector(passwords)
  df$string.id <- passwords

  return(df)

}
