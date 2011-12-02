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


