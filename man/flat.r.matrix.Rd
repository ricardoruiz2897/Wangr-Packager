% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flat.r.matrix.R
\name{flat.r.matrix}
\alias{flat.r.matrix}
\title{Simple formatting of a correlation matrix}
\usage{
flat.r.matrix(.df)
}
\arguments{
\item{df}{Correlation matrix}
}
\value{
Table containing four columns:
Column 1 : row names (variable 1 for the correlation test)
Column 2 : column names (variable 2 for the correlation test)
Column 3 : the correlation coefficients
Column 4 : the p-values of the correlations
}
\description{
This function provides a simple formatting of a correlation matrix
into a table.

Modified from https://rpubs.com/MajstorMaestro/240657
}
\examples{
flat.r.matrix(correlation.matrix)
}
