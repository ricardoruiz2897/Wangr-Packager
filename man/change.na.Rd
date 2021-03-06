% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/change.na.R
\name{change.na}
\alias{change.na}
\title{Change NA}
\usage{
change.na(df, column, change.to = 0)
}
\arguments{
\item{column}{column name to change NAs}

\item{change.to}{value to change na to (default is 0)}
}
\value{
the return value is what NA is changed to
}
\description{
This changes NA values; the default is to change NA's to 0
}
\examples{
df$col <- change.na(df$col, change.to = 0)
df$col <- change.na(df$col, change.to = -999)

}
