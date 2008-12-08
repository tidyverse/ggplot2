# Expand range
# Convenience function for expanding a range with a multiplicative or additive constant.
# 
# @arguments range of data
# @arguments multiplicative constract
# @arguments additive constant
# @keyword manip 
expand_range <- function(range, mul = 0, add = 0, zero = 0.5) {
  if (length(range) == 1 || diff(range) == 0) {
    c(range[1] - zero, range[1] + zero)
  } else {    
    range + c(-1, 1) * (diff(range) * mul + add)
  }
}