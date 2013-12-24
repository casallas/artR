\name{ART}
\alias{ART}
\title{Perform the Aligned Rank Transform on the given data frame}
\usage{
ART(longDF)
}
\arguments{
  \item{longDF}{A "long format" Data Frame}
}
\value{
A data frame containing, in addition to the original
columns, the Aligned and Ranked columns
}
\description{
This function calls \code{\link{CreateNewColumns}},
\code{\link{ComputeAlignedColumns}},
\code{\link{CheckAlignedColumns}}, and
\code{\link{ComputeRankedColumns}}
}
\details{
If an aligned column doesn't add up to zero, a warning is
displayed
}

