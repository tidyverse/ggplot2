#' Size scale.
#'
#' @name scale_size
#' @inheritParams scale_x_continuous
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum size of the plotting symbol after transformation.
#' @examples
#' \donttest{
#' (p <- qplot(mpg, cyl, data=mtcars, size=cyl))
#' p + scale_size("cylinders")
#' p + scale_size("number\nof\ncylinders")
#'
#' p + scale_size(range = c(0, 10))
#' p + scale_size(range = c(1, 2))
#'
#' # Map area, instead of width/radius
#' # Perceptually, this is a little better
#' p + scale_size_area()
#' p + scale_size_area(max_size = 25)
#'
#' # Also works with factors, but not a terribly good
#' # idea, unless your factor is ordered, as in this example
#' qplot(mpg, cyl, data=mtcars, size=factor(cyl))
#'
#' # To control the size mapping for discrete variable, use
#' # scale_size_manual:
#' last_plot() + scale_size_manual(values=c(2,4,6))
#' }
NULL

#' @rdname scale_size
#' @export
scale_size_continuous <- function(..., range = c(1, 6)) {
  continuous_scale("size", "size_c", rescale_pal(range), ...)
}

#' @rdname scale_size
#' @export
scale_size <- scale_size_continuous

#' @rdname scale_size
#' @export
scale_size_discrete <- function(..., range = c(1, 6)) {
  discrete_scale("size", "size_d",
    function(n) seq(range[1], range[2], length = n), ...)
}

#' Scale area instead of radius, for size.
#'
#' When \code{scale_size_area} is used, the default behavior is to scale the
#' area of points to be proportional to the value.
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
