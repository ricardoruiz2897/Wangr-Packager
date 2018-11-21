#'Triangle of Correlation Matrix
#'
#'@description
#'Creates a specified triangle (upper or lower) of a correlation matrix and saves it into html or latez format
#'with or without "stars" (asterisks).
#'
#'modified from
#'http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#'
#'@usage
#'rtable(.x, method, removeTriangle, result, stars)
#'
#'@param x Correlation matrix.
#'@param method Correlation method. "pearson"" or "spearman"" is supported. Defaults to "pearson".
#'@param removeTriangle remove "upper" or "lower" triangle. Defaults to "upper"
#'@param results "html" or "latex". Defaults to html
#'@param stars True if want to see stars, else False. Defaults to False.
#'
#'@return Table displayed in html or latex format.
#'
#'@examples
#'#Pearson, remove upper triangle, html, no stars
#'rtable(correlationMatrix, "pearson", "upper","html", False)
#'
#'#Spearman, remove lower triangle, latex, with stars
#'rtable(correlationMatrix, "spearman", "lower", "latex", True)
#'
rtable <-function(x, method = "pearson", # or "spearman"
                  removeTriangle = "upper", # or "lower"
                  result = "html", # or "none" or "latex"
                  stars = FALSE){
  #Compute correlation matrix
  if(!require(Hmisc)) {
    message("installing the 'Hmisc' package")
    install.packages("Hmisc")
  }
  if(!require(xtable)) {
    message("installing the 'xtable' package")
    install.packages("xtable")
  }
  #require(Hmisc)
  #require(xtable)
  x <- as.matrix(x)
  correlation_matrix <- Hmisc::rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  # If stars are to be printed.
  if(stars){
    ## Define notions for significance levels; spacing is important.
    mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  }

  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  ## build a new matrix that includes the correlations with their apropriate stars, if
  ## stars is set to True otherwise just make the matrix.
  if(stars){
    Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
  }
  else{
    Rnew <- matrix(R, ncol = ncol(x))
  }
  diag(Rnew) <- paste(diag(R), " ", sep = "")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep = "")

  ## remove upper triangle of correlation matrix
  if(removeTriangle[1] == "upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1] == "lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1] == "none") return(Rnew)
  else{
    if(result[1] == "html") print(xtable(Rnew), type = "html")
    else print(xtable(Rnew), type = "latex")
  }
}


# Example
# rtable(mtcars[,1:7], result = "html")
# rtable(mtcars[,1:7], result = "html", stars = T)

