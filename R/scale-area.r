#' Scale area instead of radius (for size).
#'
#' \code{\link{scale_area}} is deprecated and will be removed in a future
#' version of ggplot2. Use \code{\link{scale_size_area}} instead. Note that the
#' default behavir of \code{\link{scale_size_area}} is slightly different: by
#' default, it makes the area proportional to the numeric value.
#'
#' @param ... Other arguments passed on to \code{\link{continuous_scale}}
#'   to control name, limits, breaks, labels and so forth.
#' @param range Range of output sizes.  Should be greater than 0.
#' @export
scale_area <- function(..., range = c(1, 6)) {
  gg_dep("0.9.2", paste(sep = "\n",
    "scale_area is deprecated. Use scale_size_area instead.",
    "  Note that the behavior of scale_size_area is slightly different:",
    "  by default it makes the area proportional to the numeric value."))
  continuous_scale("size", "area", area_pal(range), ...)
}
