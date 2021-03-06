% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str.to.vect.str.R
\name{str.to.vect.str}
\alias{str.to.vect.str}
\title{String to Vector String.}
\usage{
\method{str}{to.vect.str}(str, pattern = ",")
}
\arguments{
\item{str}{A string to be part into a vector.}

\item{pattern}{A pattern where we will divide the string.(Defaults to ",").}
}
\value{
A vector of strings with parting from the input string and the pattern.

str.to.vect.str("three,two,one", pattern=",")
str.to.vect.str("three/two/one", pattern="/")
}
\description{
Returns a vector of strings parting a giving string given a pattern.
}
