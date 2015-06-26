#' Scale size (area or radius).
#'
#' \code{scale_size} scales area, \code{scale_radius} scales radius. The size
#' aesthetic is most commonly used for points and text, and humans percieve
#' the area of points (not their radius), so this provides for optimal
#' perception.
#'
#' @name scale_size
#' @inheritParams continuous_scale
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum size of the plotting symbol after transformation.
#' @seealso \code{\link{scale_size_area}} if you want 0 values to be mapped
#'   to points with size 0.
#' @examples
#' \donttest{
#' p <- ggplot(mtcars, aes(mpg, cyl, size = cyl)) +
#'    geom_point()
#' p
#' p + scale_size("cylinders")
#' p + scale_size("number\nof\ncylinders")
#'
#' p + scale_size(range = c(0, 10))
#' p + scale_size(range = c(1, 2))
#'
#' # If you want zero value to have zero size, use scale_size_area:
#' p + scale_size_area()
#'
#' # You can map size to a factor, but it's not a terribly good
#' # idea, unless your factor is ordered, as in this example
#' ggplot(mtcars, aes(mpg, cyl, size = factor(cyl))) +
#'   geom_point()
#'
#' # To control the size mapping for discrete variable, use
#' # scale_size_manual:
#' ggplot(mtcars, aes(mpg, cyl, size = factor(cyl))) +
#'   geom_point() +
#'   scale_size_manual(values = c(2, 4, 6))
#' }
NULL

#' @rdname scale_size
#' @export
scale_size_continuous <- function(name = NULL, breaks = waiver(), labels = waiver(),
                                  limits = NULL, range = c(1, 6),
                                  trans = "identity", guide = "legend") {
  continuous_scale("size", "area", area_pal(range), name = name,
    breaks = breaks, labels = labels, limits = limits, trans = trans,
    guide = guide)
}

#' @rdname scale_size
#' @export
scale_radius <- function(name = NULL, breaks = waiver(), labels = waiver(),
                         limits = NULL, range = c(1, 6),
                         trans = "identity", guide = "legend") {
  continuous_scale("size", "size_c", area_pal(range), name = name,
    breaks = breaks, labels = labels, limits = limits, trans = trans,
    guide = guide)
}

#' @rdname scale_size
#' @export
scale_size <- scale_size_continuous

#' @rdname scale_size
#' @export
#' @param ... Additional arguments passed on to \code{\link{discrete_scale}}.
scale_size_discrete <- function(..., range = c(2, 6)) {
  discrete_scale("size", "size_d", function(n) {
    area <- seq(range[1] ^ 2, range[2] ^ 2, length.out = n)
    sqrt(area)
  }, ...)
}

#' Scale area, with fixed 0.
#'
#' This scale is subtly different to \code{scale_area}: it ensures that a
#' value of zero is always mapped to size 0.
#'
#' Note that this controls the size scale, so it will also control
#' the thickness of lines. Line thickness will be proportional to the square
#' root of the value, which is probably undesirable in most cases.
#'
#' @param ... Other arguments passed on to \code{\link{continuous_scale}}
#'   to control name, limits, breaks, labels and so forth.
#' @param max_size Size of largest points.
#' @export
scale_size_area <- function(..., max_size = 6) {
  continuous_scale("size", "area",
    palette = abs_area(max_size),
    rescaler = rescale_max, ...)
}
