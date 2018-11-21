#'Simple formatting of a correlation matrix
#'
#'@description
#'This function provides a simple formatting of a correlation matrix
#'into a table.
#'
#'Modified from https://rpubs.com/MajstorMaestro/240657
#'
#'@usage
#'flat.r.matrix(.df)
#'
#'@param df Correlation matrix
#'
#'@return
#'Table containing four columns:
#'Column 1 : row names (variable 1 for the correlation test)
#'Column 2 : column names (variable 2 for the correlation test)
#'Column 3 : the correlation coefficients
#'Column 4 : the p-values of the correlations
#'
#'@examples
#'flat.r.matrix(correlation.matrix)
# Modified from https://rpubs.com/MajstorMaestro/240657
flat.r.matrix <- function(df){
  if(!require(tidyr)) {
    message("installing the 'tidyr' package")
    install.packages("tidyr")
  }
  if(!require(tibble)) {
    message("installing the 'tibble' package")
    install.packages("tibble")
  }
  if(!require(Hmisc)) {
    message("installing the 'Hmisc' package")
    install.packages("Hmisc")
  }
  if(!require(dplyr)) {
    message("installing the 'dplyr' package")
    install.packages("dplyr")
  }
  #  library(tidyr)
  #  library(tibble)
  #  library(Hmisc)
  #  library(dplyr)

  df.r <- Hmisc::rcorr(as.matrix(df))
  df.r$r <- round(df.r$r, 2) # round to 2 digits; can change
  df.r$P <- round(df.r$P, 2)
  df.r$r[upper.tri(df.r$r, diag = TRUE)] <- "" # keep only lower triangle

  df.r$r <- tibble::rownames_to_column(as.data.frame(df.r$r), var = "row")
  df.r$r <- tidyr::gather(df.r$r, column, cor, -1)
  df.r$P <- tibble::rownames_to_column(as.data.frame(df.r$P), var = "row")
  df.r$P <- tidyr::gather(df.r$P, column, p, -1)
  df.r.matrix <- dplyr::left_join(df.r$r, df.r$P, by = c("row", "column"))
  df.r.matrix <- df.r.matrix %>%
    dplyr::filter(cor >= 0) # filters out NAs created by keeping only lower triangle
  df.r.matrix
}
