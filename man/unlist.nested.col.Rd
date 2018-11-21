\name{unlist.nested.col}
\alias{unlist.nested.col}
\title{
unlist.nested.col
}
\description{
Unlists a nested column. Will replace NULL values during the process.
}
\usage{
unlist.nested.col(df, nested.col, replace.null.with = NA)
}

\arguments{
  \item{df}{
    Dataframe with nested column.
  }
  \item{nested.col}{
    Name of nested column.
  }
  \item{replace.null.with}{Value you want to replace NULL's with. Defaults to NA.}
}
\note{
Requires dplyr.
}


\examples{
#Let nested.col be a nested column in dataframe df.

unlist.nested.col(df, nested.col)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
