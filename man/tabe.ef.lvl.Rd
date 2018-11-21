\name{Tabe.ef.lvl}
\alias{Tabe.ef.lvl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tabe.ef.lvl}
\description{Takes a column of scores and returns a 1-6 score system for those values.}
\usage{
Tabe.ef.lvl(.df, .col.names)
}

\arguments{
  \item{df}{The dataframe containing the column you are using.}
  \item{col.names}{The name of the column containing the values you want scored.}
}

\value{
  \item{1}{Score for values that are less than 2.}
  \item{2}{Score for values that are between 2 and 4.}
  \item{3}{Score for values that are between 4 and 6.}
  \item{4}{Score for values that are between 6 and 9.}
  \item{5}{Score for values that are between 9 and 11.}
  \item{6}{Score for values that are greater than 11.}
}
