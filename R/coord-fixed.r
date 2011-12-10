#' Cartesian coordinates with fixed relationship between x and y scales.
#'
#' A fixed scale coordinate system forces a specified ratio between the
#' physical representation of data units on the axes. The ratio represents the
#' number of units on the y-axis equivalent to one unit on the x-axis. The
#' default, ratio = 1, ensures that one unit on the x-axis is the same length
#' as one unit on the y-axis. Ratios higher than one make units on the y axis
#' longer than units on the x-axis, and vice versa. This is similar to
#' \code{\link[MASS]{eqscplot}}, but it works for all types of graphics.
#'
#' @aliases coord_fixed coord_equal
#' @export coord_fixed coord_equal
#' @inheritParams coord_cartesian
#' @examples
#' # ensures that the ranges of axes are equal to the specified ratio by
#' # adjusting the plot aspect ratio
#' 
#' qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 1)
#' qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 5)
#' qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 1/5)
#' 
#' # Resize the plot to see that the specified aspect ratio is mantained
coord_fixed <- function(ratio = 1, xlim = NULL, ylim = NULL, wise = FALSE) {
  coord(limits = list(x = xlim, y = ylim), ratio = ratio, wise = wise,
    subclass = c("fixed", "cartesian"))
}
coord_equal <- coord_fixed

#' @S3method coord_aspect fixed
coord_aspect.fixed <- function(coord, ranges) {
  diff(ranges$y.range) / diff(ranges$x.range) * coord$ratio
}

icon.fixed <- function() textGrob("=", gp = gpar(cex=3))
