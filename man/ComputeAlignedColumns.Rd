\name{ComputeAlignedColumns}
\alias{ComputeAlignedColumns}
\title{Compute the aligned columns for the given data frame}
\usage{
ComputeAlignedColumns(srcTable, dstCols)
}
\arguments{
  \item{srcTable}{The source data table from which the
  calculations are made.}

  \item{dstCols}{The destination columns into which the
  aligned data is put.}
}
\description{
Computes the aligned data and adds that data to the aligned
columns in the destination data table.
}
\details{
This alignment procedure follows that of statistician J.J.
Higgins, who worked out the mathematics for an N-way
factorial and communicated them to Jacob Wobbrock in a
personal whitepaper, who translated his mathematics into
the algorithm in ARTool.
}

