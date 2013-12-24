\name{FactorNames}
\alias{FactorNames}
\title{Get the factor names of a "long format" data frame}
\usage{
FactorNames(longDF)
}
\arguments{
  \item{longDF}{data frame in "long format"}
}
\value{
a character vector containing the names of the factor
columns
}
\description{
Gets the names of the factor columns. By default, the first
column is Subject, the last column is the response, and the
intermediate columns are factors.
}

