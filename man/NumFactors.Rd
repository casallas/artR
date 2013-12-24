\name{NumFactors}
\alias{NumFactors}
\title{Get the number of factors of a "long format" data frame}
\usage{
NumFactors(longDF)
}
\arguments{
  \item{longDF}{data frame in "long format"}
}
\value{
the number of factor columns: \code{\link{ncol}(longDF)-2}
}
\description{
Gets the number of factor columns. By default, the first
column is Subject, the last column is the response, and the
intermediate columns are factors.
}

