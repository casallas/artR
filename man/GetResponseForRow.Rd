\name{GetResponseForRow}
\alias{GetResponseForRow}
\title{Get the response for the given row}
\usage{
GetResponseForRow(longDF, row)
}
\arguments{
  \item{longDF}{data frame in "long format"}

  \item{row}{The 1-based row number of the response to
  get.}
}
\value{
The response for row:
\code{longDF[row,\link{ncol}(longDF)]}
}
\description{
Gets the Y response value for the given 1-based row number.
This value is assumed to be numeric. If it is not, an
exception is thrown. By default, the first column is
Subject, the last column is the response, and the
intermediate columns are factors.
}
\details{
Note: If the caller intends to compare the decimal value
returned by this method to other decimal values, he might
consider first casting it to a double so as to avoid
precision problems at the very end of the decimal.
}

