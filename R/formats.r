# Comma formatter
# Format number with commas separating thousands
# 
# @arguments numeric vector to format
# @arguments other arguments passed on to \code{\link{format}}
comma <- function(x, ...) {
  format(x, big.mark = ",", trim = TRUE, scientific = FALSE, ...)
}

# Currency formatter
# Round to nearest cent and display dollar sign
# 
# @arguments numeric vector to format
# @arguments other arguments passed on to \code{\link{format}}
dollar <- function(x, ...) {
  x <- round_any(x, 0.01)
  nsmall <- if (max(x) < 100) 2 else 0
  paste("$", comma(x, nsmall = nsmall), sep="")
}

# Percent formatter
# Multiply by one hundred and display percent sign
# 
# @arguments numeric vector to format
percent <- function(x) {
  x <- round_any(x, precision(x) / 10)
  paste(comma(x * 100), "%", sep="")
}

# Scientific formatter
# Default scientific formatting
# 
# @arguments numeric vector to format
scientific <- function(x) {
  format(x, trim = TRUE)
}

# Compute precision
# Compute precision (in power of 10) of a vector of numbers
# 
# @keyword internal
precision <- function(x) {
  10 ^ floor(log10(diff(range(x))))
}