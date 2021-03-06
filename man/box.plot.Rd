% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box.plot.R
\name{box.plot}
\alias{box.plot}
\title{Box Plot}
\usage{
box.plot(dat, cat.out, cont.pre, alpha = 0.5, jitter = F, filename,
  plot.percentage = 100)
}
\arguments{
\item{dat}{Dataframe to take info from.}

\item{jitter}{Jitter Points (Logical)}

\item{filename}{Name of the file.}

\item{cont.out}{List of continuous predictors.}

\item{cat.pre}{List of categorical outcomes.}
}
\value{
Saves a box plot in working directory on a pdf with the given filename
}
\description{
This function takes a list of categorical predictors and continuous outcomes to create a box plot.
}
\examples{
box.plot(data=df, cont.out = continous.outcomes, cat.pre = categorical.predictors, alpha=1, jitter=F, filename="Density Plot")
}
