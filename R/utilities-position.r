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

# Trim infinite.
# Trim non-finite numbers to specified range
# 
# @keyword internal
# @alias trim_infinite_01
trim_infinite <- function(x, range) {
  x[x == -Inf] <- range[1]
  x[x == Inf] <- range[2]
  x
}

trim_infinite_01 <- function(x) {
  trim_infinite(x, c(0, 1))
}


