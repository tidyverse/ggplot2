

# Resolution
# Compute the "resolution" of a data vector, ie. what is the smallest non-zero
# distance between adjacent values.
#
# @arguments numeric vector
# @keyword hplot
# @keyword internal 
resolution <- function(x) {
  un <- unique(as.numeric(x))
  
  if (length(un) == 1) return(1)
  min(diff(sort(un)))
}
