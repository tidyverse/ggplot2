#' Scale area instead of radius (for size).
#'
#' @export
scale_area <- function(..., range = c(1, 6)) {
  continuous_scale("size", "area", area_pal(range), ...)
}