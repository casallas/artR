\name{GetMeanForLevelOfFactor}
\alias{GetMeanForLevelOfFactor}
\title{Get the mean of Y for all levels of the given factor}
\usage{
GetMeanForLevelOfFactor(longDF, factor, level)
}
\arguments{
  \item{longDF}{data frame in "long format"}

  \item{factor}{The factor of interest.}

  \item{level}{The corresponding level for the given
  factor.}
}
\value{
The average Y response for the level of the given factor.
}
\description{
Gets the mean of the Y response column for all cells in
which the given factor equals the given level. That is,
level 'level' is a level of factor 'factor' in this table.
}
\details{
Note: If the Y response column, i.e., the last column in
the table, is non-numeric, an exception is thrown.
}

