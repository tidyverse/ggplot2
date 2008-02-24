# Expand range
# Convenience function for expanding a range with a multiplicative or additive constant.
# 
# @arguments range of data
# @arguments multiplicative constract
# @arguments additive constant
# @keyword manip 
expand_range <- function(range, mul=0, add=0) {
  if (diff(range) == 0) return(c(range[1] - 0.5, range[1] + 0.5))
  range + c(-1, 1) * (diff(range) * mul + add)
}