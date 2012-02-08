#' Compute the "resolution" of a data vector.
#'
#' The resolution is is the smallest non-zero distance between adjacent
#' values.  If there is only one unique value, then the resolution is defined
#' to be one. 
#' 
#' @param x numeric vector
#' @param zero should a zero value be automatically included in the
#'   computation of resolution
#' @export
#' @examples
#' resolution(1:10)
#' resolution((1:10) - 0.5)
#' resolution((1:10) - 0.5, FALSE)
#' resolution(c(1,2, 10, 20, 50))
resolution <- function(x, zero = TRUE) {
  if (zero_range(range(x, na.rm = TRUE))) return(0.5)
  
  x <- unique(as.numeric(x))
  if (zero) {
    x <- unique(c(0, x))
  }
  
  min(diff(sort(x)))
}
