\name{GetSubjectForRow}
\alias{GetSubjectForRow}
\title{Get the subject for the given row}
\usage{
GetSubjectForRow(longDF, row)
}
\arguments{
  \item{longDF}{data frame in "long format"}

  \item{row}{The 1-based row number of the subject
  identifier to get.}
}
\value{
The subject identifier for row: \code{longDF[row, 1]}
}
\description{
Gets the subject (S) for the given 1-based row number. By
default, the first column is Subject, the last column is
the response, and the intermediate columns are factors.
}

