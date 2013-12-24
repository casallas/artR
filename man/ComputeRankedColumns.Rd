\name{ComputeRankedColumns}
\alias{ComputeRankedColumns}
\title{Compute the ranked columns for the given data frame}
\usage{
ComputeRankedColumns(nFactors, dstCols)
}
\arguments{
  \item{nFactors}{The number of factors that were in the
  original source table.}

  \item{dstCols}{The destination columns whose aligned
  columns will be ranked.}
}
\description{
Takes the existing aligned columns and produces ranks for
each of those columns as new columns. The ranks are
averaged in the case of ties.
}
\seealso{
\code{\link{rank}}, which is wrapped by this function
}

