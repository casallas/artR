\name{GetMeanForLevelsOfFactors}
\alias{GetMeanForLevelsOfFactors}
\title{Get the mean of Y for all levels of the given factors}
\usage{
GetMeanForLevelsOfFactors(longDF, factors, fLevels)
}
\arguments{
  \item{longDF}{data frame in "long format"}

  \item{factors}{The factors of interest. This should
  contain one or more factor (X) names. It may also contain
  the Subject (S) column name if a subject mean is
  desired.}

  \item{levelsThe}{corresponding level for each factor.}
}
\value{
The average Y response for the levels of the given factors.
}
\description{
Gets the mean of the Y response column for all cells in
which the given factors equal the given levels. Note that
levels[i] is the desired level for factors[i].
}
\details{
Note: If the Y response column, i.e., the last column in
the table, is non-numeric, an exception is thrown.
}

