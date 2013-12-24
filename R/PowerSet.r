# R Port of WobbrockLib.Extensions.SetsEx.PowerSet C# function by Jacob Wobbrock
# (http://faculty.washington.edu/wobbrock/proj/WobbrockLib.zip)
# No license specified on the original file/library
#
# Ported to R by Juan Sebastian Casallas in 2013

#' Produces the powerset of a given set. The powerset contains 2^n items
#' for a set with n items. It includes the empty set and the original set
#' itself.
#'
#' @param The set on which to compute the powerset.</param>
#' @return The powerset of the given set. Note that factors are returned as character
#' @seealso package sets, 2^s can do it, but subset access seems more complicated
#' @export
PowerSet <- function(set)
{
  if(is.factor(set))
  {
    # Convert factors to characters to avoid problems concatenating sets of factors
    set <- as.character(set) 
  }
  size <- 2^length(set)
  pset <- vector("list",size)
  for(i in 1:size)
  {
    oset <- c()#vector("list",size)
    for(j in 1:length(set))
    {
      # Check jth bit of i
      chk <- bitwShiftR(i-1,j-1)
      if( bitwAnd(chk,0x01) == 1)
      {
        oset <- c(oset,set[j])
      }
    }
    pset[[i]] <- oset
  }
  pset
}