\name{CheckAlignedColumns}
\alias{CheckAlignedColumns}
\title{Check the sum of the aligned columns}
\usage{
CheckAlignedColumns(dstCols)
}
\arguments{
  \item{dstCols}{The columns in which to sum its aligned
  columns, which need to have been already created.}
}
\value{
A vector, where TRUE, represents columns that are
problematic.
}
\description{
Sums the aligned columns in the given table and ensures
that they sum to near-zero.
}

