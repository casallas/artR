# R Port of the ARTool (C#) by Jacob Wobbrock (https://depts.washington.edu/aimgroup/proj/art/)
# Ported to R by Juan Sebastian Casallas in 2013
# License details in the LICENSE file
source("PowerSet.r") # PowerSet()

# <summary>
# Gets the names of the factor columns. By default, the first column
# is Subject, the last column is the response, and the intermediate
# columns are factors.
# </summary>
FactorNames <- function(longDF)
{
	# Exclude (-) the first and last column names
	colnames(longDF)[c(-1,-ncol(longDF))]
}

# <summary>
# Gets the number of factor columns. By default, the first column
# is Subject, the last column is the response, and the intermediate
# columns are factors.
# </summary>
# <returns>ncol(longDF)-2</returns>
NumFactors <- function(longDF)
{
	# Exclude the first and last column names
	ncol(longDF)-2
}

# <summary>
# Gets the name of the response column. By default, the first column
# is Subject, the last column is the response, and the intermediate
# columns are factors.
# </summary>
ResponseName <- function(longDF) # assumes the last column in the table is the Y outcome (d.v.)
{
	if(ncol(longDF)>0) tail(colnames(longDF),1) else ""
}

# <summary>
# Computes the grand mean over the last column in the table. If the last
# column is not numeric, an exception is thrown. By default, the first 
# column is Subject, the last column is the response, and the intermediate
# columns are factors.
# </summary>
# <returns>The mean of the last column in the table.</returns>
GetGrandMean <- function(longDF)
{
	mean(longDF[,ncol(longDF)])
}

# <summary>
# Gets the subject (S) for the given 1-based row number. By default, the first 
# column is Subject, the last column is the response, and the intermediate
# columns are factors.
# </summary>
# <param name="row">The 1-based row number of the subject identifier to get.</param>
# <returns>The subject identifier row this row.</returns>
GetSubjectForRow <- function(longDF, row)
{
	longDF[row,1]
}

# <summary>
# Gets the Y response value for the given 1-based row number. This value is
# assumed to be numeric. If it is not, an exception is thrown. By default, the 
# first  column is Subject, the last column is the response, and the intermediate
# columns are factors.
# </summary>
# <param name="row">The 1-based row number of the response to get.</param>
# <returns>A numeric response value.</returns>
# <remarks>If the caller intends to compare the decimal value returned by
# this method to other decimal values, he might consider first casting it
# to a double so as to avoid precision problems at the very end of the decimal.</remarks>
GetResponseForRow <- function(longDF, row)
{
	longDF[row,ncol(longDF)]
}

# <summary>
# Gets a list of the values of the factors (X's) for the given 1-based row number. Any 
# numeric values will be returned as strings. By default, the first column is Subject, 
# the last column is the response, and the intermediate columns are factors.
# </summary>
# <param name="row">The 1-based row number whose values are to be gotten.</param>
# <returns>A list of string values or null if two or fewer columns exist in this table.</returns>
GetLevelsOfFactorsForRow <- function(longDF, row)
{
	simplify2array(longDF[row,FactorNames(longDF)])
}

# <summary>
# Gets the mean of the Y response column for all cells in which the given factors equal
# the given levels. Note that levels[i] is the desired level for factors[i].
# </summary>
# <param name="factors">The factors of interest. This should contain one or more factor (X) 
# names. It may also contain the Subject (S) column name if a subject mean is desired.</param>
# <param name="levels">The corresponding level for each factor.</param>
# <returns>The average Y response for the levels of the given factors.</returns>
# <remarks>If the Y response column, i.e., the last column in the table, is non-numeric, 
# an exception is thrown.</remarks>
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

# <summary>
# Gets the mean of the Y response column for all cells in which the given factor equals
# the given level. That is, level 'level' is a level of factor 'factor' in this table.
# </summary>
# <param name="factor">The factor of interest.</param>
# <param name="level">The corresponding level for the given factor.</param>
# <returns>The average Y response for the level of the given factor.</returns>
# <remarks>If the Y response column, i.e., the last column in the table, is non-numeric, 
# an exception is thrown.</remarks>
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

# <summary>
# Creates the new columns for the output data table. These will contain the aligned
# columns and ranked columns. The names for the new columns are based on the factor
# and response column names.
# </summary>
# <param name="srcTable">The source data table from which we'll create the column names
# in the destination data table.</param>
# <returns>The list of columns added for the outputted data. No actual
# output data values have been added to the columns at this point.</returns>
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

# <summary>
# Sorts a power set by the number of 
# elements that it contains, i.e., its cardinality.
# </summary>
CardinalitySort <- function(powerSet)
{
	powerSet[
		order( # Gives the ascending order based on cardinality
			sapply(powerSet,length) # Gets the number of elements in powerSet[i]
		)
	]
}

# <summary>
# Computes the aligned data and adds that data to the aligned columns in the destination
# data table.
# </summary>
# <param name="srcTable">The source data table from which the calculations are made.</param>
# <param name="dstCols">The destination columnss into which the aligned data is put.</param>
# <remarks>This alignment procedure follows that of statistician J.J. Higgins, who worked out
# the mathematics for an N-way factorial and communicated them to me in a personal whitepaper. I
# translated his mathematics into the algorithm here.</remarks>
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

# <summary>
# Takes the existing aligned columns and produces ranks for each of those columns
# as new columns. The ranks are averaged in the case of ties.
# </summary>
# <param name="nFactors">The number of factors that were in the original source table.</param>
# <param name="dstCols">The destination columns whose aligned columns will be ranked.</param>
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

# <summary>
# Sums the aligned columns in the given table and ensures that they sum to near-zero.
# </summary>
# <param name="dstCols">The columns in which to sum its aligned columns, which need to 
# have been already created.</param>
# <returns>A vector, where TRUE, represents columns that are problematic</returns>
CheckAlignedColumns <- function(dstCols)
{
	# Sum over aligned columns, which should only be half of the destination columns
	colSums <- sapply(dstCols[1:(length(dstCols)/2)],sum)
	epsilon <- 1e-12
	abs(colSums) > epsilon
}

# <summary>
# Performs the Aligned Rank Transform on the given dataset
# </summary>
# <param name="longDF">A "long format" Data Frame</param>
# <returns>A data frame containing, in addition to the original columns,
# the Aligned and Ranked columns</returns>
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
