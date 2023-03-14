#' Compute the "resolution" of a numeric vector
#'
#' The resolution is the smallest non-zero distance between adjacent
#' values.  If there is only one unique value, then the resolution is defined
#' to be one. If x is an integer vector, then it is assumed to represent a
#' discrete variable, and the resolution is 1.
#'
#' @param x numeric vector
#' @param zero should a zero value be automatically included in the
#'   computation of resolution
#' @export
#' @examples
#' resolution(1:10)
#' resolution((1:10) - 0.5)
#' resolution((1:10) - 0.5, FALSE)
#'
#' # Note the difference between numeric and integer vectors
#' resolution(c(2, 10, 20, 50))
#' resolution(c(2L, 10L, 20L, 50L))
resolution <- function(x, zero = TRUE) {
  if (is.integer(x) || zero_range(range(x, na.rm = TRUE)))
    return(1)

  x <- unique0(as.numeric(x))
  if (zero) {
    x <- unique0(c(0, x))
  }

  min(diff(sort(x)))
}
