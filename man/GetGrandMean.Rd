\name{GetGrandMean}
\alias{GetGrandMean}
\title{Get the grand mean of the response column}
\usage{
GetGrandMean(longDF)
}
\arguments{
  \item{longDF}{data frame in "long format"}
}
\value{
The mean of the last column in the table:
\code{\link{mean}longDF[, \link{ncol}(longDF)]}
}
\description{
Computes the grand mean over the last column in the table.
If the last column is not numeric, an exception is thrown.
By default, the first column is Subject, the last column is
the response, and the intermediate columns are factors.
}

