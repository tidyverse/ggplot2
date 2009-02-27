# Resolution
# Compute the "resolution" of a data vector, ie. what is the smallest non-zero
# distance between adjacent values.
#
# If there is only one unique value, then the resolution is defined to be one. 
# 
# @arguments numeric vector
# @arguments should a zero value be automatically included in the computation of resolution
# @keyword hplot
# @keyword internal 
#X resolution(1:10)
#X resolution((1:10) - 0.5)
#X resolution((1:10) - 0.5, FALSE)
#X resolution(c(1,2, 10, 20, 50))
resolution <- function(x, zero = TRUE) {
  x <- unique(as.numeric(x))
  if (length(x) == 1) return(1)

  if (zero) {
    x <- unique(c(0, x))
  }
  
  min(diff(sort(x)))
}