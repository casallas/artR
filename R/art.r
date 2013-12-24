# R Port of the ARTool (C#) by Jacob Wobbrock (https://depts.washington.edu/aimgroup/proj/art/)
# Ported to R by Juan Sebastian Casallas in 2013
# License details in the LICENSE file

#' Get the factor names of a "long format" data frame
#'
#' Gets the names of the factor columns. By default, the first column
#' is Subject, the last column is the response, and the intermediate
#' columns are factors.
#'
#' @param longDF data frame in "long format"
#' @return a character vector containing the names of the factor columns
FactorNames <- function(longDF)
{
	# Exclude (-) the first and last column names
	colnames(longDF)[c(-1,-ncol(longDF))]
}

#' Get the number of factors of a "long format" data frame
#'
#' Gets the number of factor columns. By default, the first column
#' is Subject, the last column is the response, and the intermediate
#' columns are factors.
#'
#' @param longDF data frame in "long format"
#' @return the number of factor columns: \code{\link{ncol}(longDF)-2}
NumFactors <- function(longDF)
{
	# Exclude the first and last column names
	ncol(longDF)-2
}

#' Get the name of the response column
#'
#' Gets the name of the response column. By default, the first column
#' is Subject, the last column is the response, and the intermediate
#' columns are factors.
#'
#' @param longDF data frame in "long format"
#' @return the name of the last column of longDF: \code{\link{tail}(\link{colnames}(longDF), 1)}
ResponseName <- function(longDF) # assumes the last column in the table is the Y outcome (d.v.)
{
	if(ncol(longDF)>0) tail(colnames(longDF),1) else ""
}

#' Get the grand mean of the response column
#'
#' Computes the grand mean over the last column in the table. If the last
#' column is not numeric, an exception is thrown. By default, the first 
#' column is Subject, the last column is the response, and the intermediate
#' columns are factors.
#'
#' @param longDF data frame in "long format"
#' @return The mean of the last column in the table: \code{\link{mean}longDF[, \link{ncol}(longDF)]}
GetGrandMean <- function(longDF)
{
	mean(longDF[,ncol(longDF)])
}

#' Get the subject for the given row
#'
#' Gets the subject (S) for the given 1-based row number. By default, the first 
#' column is Subject, the last column is the response, and the intermediate
#' columns are factors.
#'
#' @param longDF data frame in "long format"
#' @param row The 1-based row number of the subject identifier to get.
#' @return The subject identifier for row: \code{longDF[row, 1]}
GetSubjectForRow <- function(longDF, row)
{
	longDF[row,1]
}

#' Get the response for the given row
#'
#' Gets the Y response value for the given 1-based row number. This value is
#' assumed to be numeric. If it is not, an exception is thrown. By default, the 
#' first  column is Subject, the last column is the response, and the intermediate
#' columns are factors.
#'
#' Note: If the caller intends to compare the decimal value returned by
#' this method to other decimal values, he might consider first casting it
#' to a double so as to avoid precision problems at the very end of the decimal.
#'
#' @param longDF data frame in "long format"
#' @param row The 1-based row number of the response to get.
#' @return The response for row: \code{longDF[row,\link{ncol}(longDF)]}
GetResponseForRow <- function(longDF, row)
{
	longDF[row,ncol(longDF)]
}

#' Get the levels of the factors of the given row
#'
#' Gets a list of the values of the factors (X's) for the given 1-based row number. Any 
#' numeric values will be returned as strings. By default, the first column is Subject, 
#' the last column is the response, and the intermediate columns are factors.
#'
#' @param longDF data frame in "long format"
#' @param row The 1-based row number whose values are to be gotten.
#' @return A vector of string values: \code{\link{simplify2array}(longDF[row, \link{FactorNames}(longDF)])}
GetLevelsOfFactorsForRow <- function(longDF, row)
{
	simplify2array(longDF[row,FactorNames(longDF)])
}

#' Get the mean of Y for all levels of the given factors
#'
#' Gets the mean of the Y response column for all cells in which the given factors equal
#' the given levels. Note that levels[i] is the desired level for factors[i].
#'
#' Note: If the Y response column, i.e., the last column in the table, is non-numeric, 
#' an exception is thrown.
#'
#' @param longDF data frame in "long format"
#' @param factors The factors of interest. This should contain one or more factor (X) 
#' names. It may also contain the Subject (S) column name if a subject mean is desired.
#' @param levelsThe corresponding level for each factor.
#' @return The average Y response for the levels of the given factors.
GetMeanForLevelsOfFactors <- function(longDF, factors, fLevels)
{
	if(is.factor(factors)|is.factor(fLevels))
	{
		# Convert factors to characters to avoid problems comparing levels of factors
		factors <- as.character(factors)
		fLevels <- as.character(fLevels)
	}	
	
	# final row set to compute over
	# a boolean vector works, since rows can only be selected once
    forRows <- rep(TRUE,nrow(longDF)) #  at the beginning no rows are selected (all false)
    for (f in 1:length(factors))
    {
		rows <- longDF[[factors[f]]] == fLevels[[f]]
		forRows <- forRows & rows # Update the rowset
    }
	yCol <- longDF[,ncol(longDF)]
	stopifnot(is.numeric(yCol)) # Unable to compute mean on non-numeric response column yCol
    mean(yCol[forRows])
}

#' Get the mean of Y for all levels of the given factor
#'
#' Gets the mean of the Y response column for all cells in which the given factor equals
#' the given level. That is, level 'level' is a level of factor 'factor' in this table.
#'
#' Note: If the Y response column, i.e., the last column in the table, is non-numeric, 
#' an exception is thrown.
#'
#' @param longDF data frame in "long format"
#' @param factor The factor of interest.
#' @param level The corresponding level for the given factor.
#' @return The average Y response for the level of the given factor.
GetMeanForLevelOfFactor <- function(longDF, factor, level)
{
	# This method patches in to the more general version that takes N factors and N levels.
	factors <- list(factor)
	levels <- list(level)
	GetMeanForLevelsOfFactors(longDF, factors, levels)
}

# Auxiliary function to name additional columns
# returns "type(response,effect)"
.NewColName <- function(effect,response,type)
{
	# Paste0 uses "" separators
	paste0(type,".",response,".",effect)
	#paste0(type,"(",response,",",effect,")")
	# To get the same colnames as Wobbrock: Overload as paste0(type,"(",response,") for ",effect)
}

# Auxiliary function to name additional columns
# returns "aligned(response,effect)"
.AlignedColName <- function(effect,response)
{
	.NewColName(effect,response,"aligned")
}

# Auxiliary function to name additional columns
# returns "ART(response,effect)"
.ARTColName <- function(effect,response)
{
	.NewColName(effect,response,"ART")
}

#' Create new columns for the output data table
#'
#' Creates the new columns for the output data table. These will contain the aligned
#' columns and ranked columns. The names for the new columns are based on the factor
#' and response column names.
#'
#' @param srcTable The source data table from which we'll create the column names
#' in the destination data table.
#' @return The list of columns added for the outputted data. No actual
#' output data values have been added to the columns at this point.
#' @export
CreateNewColumns <- function(srcTable)
{
    # create column names for all the aligned main effects and interactions
    factorPset <- PowerSet(FactorNames(srcTable)) # factor powerset
    factorPset <- sapply(factorPset[-1],paste,collapse=".x.") # Skip the empty set at [1], use collapse="*" for Wobbrock's format
	
	# Generate the column names for all the aligned main effects and interactions
	colNames <- sapply(factorPset,.AlignedColName,response=ResponseName(srcTable))

	# Append the column names for all the aligned main effects and interactions
	colNames <- c(colNames,
					sapply(factorPset,.ARTColName,response=ResponseName(srcTable))
				)

	# Generate an empty list of the size of the column names
	cols <- vector("list",length(colNames))
	# Change the names
	names(cols) <- colNames
	# Return the list
	cols
}

#' Sort a power set by cardinality
#'
#' Sorts a power set by the number of 
#' elements that it contains, i.e., its cardinality.
#'
#' @param powerSet A power set given by \code{\link{PowerSet}}
#' @return powerSet sorted by cardinality
#' @seealso \code{\link{order}} that is wrapped by this function
CardinalitySort <- function(powerSet)
{
	powerSet[
		order( # Gives the ascending order based on cardinality
			sapply(powerSet,length) # Gets the number of elements in powerSet[i]
		)
	]
}

#' Compute the aligned columns for the given data frame
#'
#' Computes the aligned data and adds that data to the aligned columns in the destination
#' data table.
#'
#' This alignment procedure follows that of statistician J.J. Higgins, who worked out
#' the mathematics for an N-way factorial and communicated them to Jacob Wobbrock in a 
#' personal whitepaper, who translated his mathematics into the algorithm in ARTool.
#'
#' @param srcTable The source data table from which the calculations are made.
#' @param dstCols The destination columns into which the aligned data is put.
#' @export
ComputeAlignedColumns <- function(srcTable, dstCols)
{
	factorPset <- PowerSet(FactorNames(srcTable))
	stopifnot(length(factorPset)==2^ncol(FactorNames(srcTable))) #Incorrect count for number of new columns
	
	#SetSizeComparer sComp = new SetSizeComparer();
	grandMean <- GetGrandMean(srcTable)
	
    # walk down each row of the data table, adjusting each point (e.g., Y -> adjY) appropriately
    # for each new main or interaction effect created for the aligned data table.
	for(r in 1:nrow(srcTable))
	{
        # S <- GetSubjectForRow(srcTable, r); # get the level of Subject for this row
        Xs <- GetLevelsOfFactorsForRow(srcTable,r); # get the levels of X's for this row
        Y <- GetResponseForRow(srcTable,r); # get Y response that we will adjust ("align")
        # retain the original columns in our new table
        # rbind.fill(dstTable,list(S,Xs,Y))) # FIX this
		stopifnot(length(Xs)+2 == ncol(srcTable)) #Number of columns do not agree
		
		# compute the residual
		cellMean <- GetMeanForLevelsOfFactors(srcTable,FactorNames(srcTable), Xs)
		residual <- Y - cellMean
		
        # levelsPset[i] corresponds to factor factorPset[i]
        levelsPset <- PowerSet(Xs)
		
        # compute effect for each factor or interaction column
        for(i in 2:length(factorPset))
        {
            efxPset <- PowerSet(factorPset[[i]]) # get powerset of efx being considered
            efxPset <- CardinalitySort(efxPset) # sorts in ascending cardinality
            lvlPset <- PowerSet(levelsPset[[i]]) # get powerset of levels being considered
            lvlPset <- CardinalitySort(lvlPset); # sorts in ascending cardinality

            efx <- 0 # the effect of interest
            cSign <- +1 # used to flip +1/-1

            # this is the J.J. Higgins alignment approach
            for (j in length(efxPset):2) # go from highest ordinality to least for subeffects
            {
				efx <- efx + (cSign * 
								GetMeanForLevelsOfFactors(srcTable,
												(efxPset[[j]]),
												(lvlPset[[j]])
								)
							)
				
				if(is.nan(efx))
				{
					stop(paste("Effect",efxPset[[j]],"is NA"))
				}
                if (length(efxPset[[j]]) != length(efxPset[[j - 1]]))
                    cSign <- -cSign # flip sign when cardinality changes
            }
            efx <- efx + (cSign * grandMean)
            adjY <- residual + efx # aligned response
            dstCols[[i-1]] <- append(dstCols[[i-1]],adjY)
        } 
	}# end of iteration down rows
	dstCols
}

#' Compute the ranked columns for the given data frame
#'
#' Takes the existing aligned columns and produces ranks for each of those columns
#' as new columns. The ranks are averaged in the case of ties.
#'
#' @param nFactors The number of factors that were in the original source table.
#' @param dstCols The destination columns whose aligned columns will be ranked.
#' @seealso \code{\link{rank}}, which is wrapped by this function
#' @export
ComputeRankedColumns <- function(nFactors, dstCols)
{
    count = (2^nFactors)-1
    for (c in 1:count)
    {
      # Round to 12 decimal places to avoid precision problems at the end of each aligned number
      alignedCol <- round(dstCols[[c]],12)
      dstCols[[c+count]] <- rank(alignedCol,ties.method="average")
    }
    dstCols
}

#' Check the sum of the aligned columns
#'
#' Sums the aligned columns in the given table and ensures that they sum to near-zero.
#'
#' @param dstCols The columns in which to sum its aligned columns, which need to 
#' have been already created.
#' @return A vector, where TRUE, represents columns that are problematic.
#' @export
CheckAlignedColumns <- function(dstCols)
{
	# Sum over aligned columns, which should only be half of the destination columns
	colSums <- sapply(dstCols[1:(length(dstCols)/2)],sum)
	epsilon <- 1e-12
	abs(colSums) > epsilon
}

#' Perform the Aligned Rank Transform on the given data frame
#'
#' This function calls \code{\link{CreateNewColumns}},
#' \code{\link{ComputeAlignedColumns}}, \code{\link{CheckAlignedColumns}},
#' and \code{\link{ComputeRankedColumns}}
#'
#' If an aligned column doesn't add up to zero, a warning is displayed
#'
#' @param longDF A "long format" Data Frame
#' @return A data frame containing, in addition to the original columns,
#' the Aligned and Ranked columns
#' @export
ART <- function(longDF)
{
	dstCols <- CreateNewColumns(longDF)
	dstCols <- ComputeAlignedColumns(longDF,dstCols)
	chkCols <- CheckAlignedColumns(dstCols)
	if(sum(chkCols)) # If at least one column has problems, print it
	{
		warning("The following aligned columns didn't add up to zero:")
		warning(paste0(names(dstCols)[chkCols],collapse="\n"))
	}
	dstCols <- ComputeRankedColumns(NumFactors(longDF),dstCols)
	cbind(longDF,dstCols)
}
