#'Clear
#'@description
#'Clears all the functions from the enviroment.
#'
#'@usage
#'clear.func(.e)
#'
#'@param e Defaults to enviroment
#'
#'@examples
#'#Delete all functions from the enviroment.
#'clear.func()
#'
#'
clear.func <- function(e=.GlobalEnv){

  remove(list=ls(e)[sapply(ls(e),function(n){is.function(get(n))})],envir=e)

}

