\name{ResponseName}
\alias{ResponseName}
\title{Get the name of the response column}
\usage{
ResponseName(longDF)
}
\arguments{
  \item{longDF}{data frame in "long format"}
}
\value{
the name of the last column of longDF:
\code{\link{tail}(\link{colnames}(longDF), 1)}
}
\description{
Gets the name of the response column. By default, the first
column is Subject, the last column is the response, and the
intermediate columns are factors.
}

