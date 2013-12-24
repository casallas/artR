\name{GetLevelsOfFactorsForRow}
\alias{GetLevelsOfFactorsForRow}
\title{Get the levels of the factors of the given row}
\usage{
GetLevelsOfFactorsForRow(longDF, row)
}
\arguments{
  \item{longDF}{data frame in "long format"}

  \item{row}{The 1-based row number whose values are to be
  gotten.}
}
\value{
A vector of string values:
\code{\link{simplify2array}(longDF[row,
\link{FactorNames}(longDF)])}
}
\description{
Gets a list of the values of the factors (X's) for the
given 1-based row number. Any numeric values will be
returned as strings. By default, the first column is
Subject, the last column is the response, and the
intermediate columns are factors.
}

