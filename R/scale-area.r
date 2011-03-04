#' Scale area instead of radius (for size).
#'
#' @param ... Other arguments passed on to \code{\link{continuous_scale}} 
#'   to control name, limits, breaks, labels and so forth.
#' @param range Range of output sizes.  Should be greater than 0.
#' @export
scale_area <- function(..., range = c(1, 6)) {
  continuous_scale("size", "area", area_pal(range), ...)
}