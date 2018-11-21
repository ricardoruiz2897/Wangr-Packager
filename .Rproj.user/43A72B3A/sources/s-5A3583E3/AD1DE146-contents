#'Change name of objects.
#'
#'@description
#'This function changes the name of an object without having to create a new one.
#'
#'@usage
#'mv(old.name, new.name)
#'
#'@param old.name The name of the object to change name.
#'@param new.name The new name for the object.
#'
#'@return
#'Name of object is changed in the enviroment.
#'
#'@examples
#'a <- data.frame()
#'
#'#Will change the name from a to c.
#'mv(a,c)
#'
# mv ----------------------------------------------------------------------
# http://r.789695.n4.nabble.com/renaming-objects-td851715.html
# like Unix mv command--used to rename objectss
mv <- function (old.name, new.name) {
  anm <- deparse(substitute(old.name))
  bnm <- deparse(substitute(new.name))
  if (!exists(anm,where=1,inherits=FALSE))
    stop(paste(anm, "does not exist.\n"))
  if (exists(bnm,where=1,inherits=FALSE)) {
    ans <- readline(paste("Overwrite ", bnm, "? (y/n) ", sep =
                            ""))
    if (ans != "y")
      return(invisible())
  }
  assign(bnm, old.name, pos = 1)
  rm(list = anm, pos = 1)
  invisible()
}

# Example:  mv(old.name, new.name)

