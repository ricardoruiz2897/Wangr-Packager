#'String to Vector String.
#'
#'@description
#'Returns a vector of strings parting a giving string given a pattern.
#'
#'@Usage
#'str.to.vect.str(str, pattern)
#'
#'@param str A string to be part into a vector.
#'@param pattern A pattern where we will divide the string.(Defaults to ",").
#'
#'@return
#'A vector of strings with parting from the input string and the pattern.
#'
#'str.to.vect.str("three,two,one", pattern=",")
#'str.to.vect.str("three/two/one", pattern="/")
#'
str.to.vect.str <- function(str, pattern = ","){

  if(!require("stringr")){
    install.packages("stringr")
    require("stringr")
  }

  str.vec <- unlist(stringr::str_split(str, pattern))

  #Remove trailing and leading white spaces if they exist.
  for(i in 1:length(str.vec)){

    str.vec[i] <- stringr::str_trim(str.vec[i], side="both")


  }

  return(str.vec)

}

