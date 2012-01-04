#' Size scale.
#' 
#' @inheritParams scale_x_continuous
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum size of the plotting symbol after transformation.
#' @rdname scale_size
#' @export
#' @examples
#' (p <- qplot(mpg, cyl, data=mtcars, size=cyl))
#' p + scale_size("cylinders")
#' p + scale_size("number\nof\ncylinders")
#' 
#' p + scale_size(range = c(0, 10))
#' p + scale_size(range = c(1, 2))
#' 
#' # Map area, instead of width/radius
#' # Perceptually, this is a little better
#' p + scale_area()
#' p + scale_area(range = c(1, 25))
#' 
#' # Also works with factors, but not a terribly good
#' # idea, unless your factor is ordered, as in this example
#' qplot(mpg, cyl, data=mtcars, size=factor(cyl))
#' 
#' # To control the size mapping for discrete variable, use 
#' # scale_size_manual:
#' last_plot() + scale_size_manual(values=c(2,4,6))
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

icon.size <- function() {
  pos <- c(0.15, 0.3, 0.5, 0.75)
  circleGrob(pos, pos, r=(c(0.1, 0.2, 0.3, 0.4)/2.5), gp=gpar(fill="grey50", col=NA))
}
