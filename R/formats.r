comma <- function(x, ...) {
  format(x, big.mark = ",", trim = TRUE, scientific = FALSE, ...)
}

dollar <- function(x, ...) {
  x <- round_any(x, 0.01)
  nsmall <- if (max(x) < 100) 2 else 0
  paste("$", comma(x, nsmall = nsmall), sep="")
}

percent <- function(x) {
  x <- round_any(x, precision(x) / 10)
  paste(comma(x * 100), "%", sep="")
}

scientific <- function(x) {
  format(x, trim = TRUE)
}

precision <- function(x) {
  10 ^ floor(log10(diff(range(x))))
}