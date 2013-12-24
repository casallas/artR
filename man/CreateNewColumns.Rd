\name{CreateNewColumns}
\alias{CreateNewColumns}
\title{Create new columns for the output data table}
\usage{
CreateNewColumns(srcTable)
}
\arguments{
  \item{srcTable}{The source data table from which we'll
  create the column names in the destination data table.}
}
\value{
The list of columns added for the outputted data. No actual
output data values have been added to the columns at this
point.
}
\description{
Creates the new columns for the output data table. These
will contain the aligned columns and ranked columns. The
names for the new columns are based on the factor and
response column names.
}

