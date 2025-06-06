#' Cartesian coordinates with fixed "aspect ratio"
#'
#' A fixed scale coordinate system forces a specified ratio between the
#' physical representation of data units on the axes. The ratio represents the
#' number of units on the y-axis equivalent to one unit on the x-axis. The
#' default, `ratio = 1`, ensures that one unit on the x-axis is the same
#' length as one unit on the y-axis. Ratios higher than one make units on the
#' y axis longer than units on the x-axis, and vice versa. This is similar to
#' [MASS::eqscplot()], but it works for all types of graphics.
#'
#' @export
#' @inheritParams coord_cartesian
#' @inheritDotParams coord_cartesian
#' @examples
#' # ensures that the ranges of axes are equal to the specified ratio by
#' # adjusting the plot aspect ratio
#'
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p + coord_fixed(ratio = 1)
#' p + coord_fixed(ratio = 5)
#' p + coord_fixed(ratio = 1/5)
#' p + coord_fixed(xlim = c(15, 30))
#'
#' # Resize the plot to see that the specified aspect ratio is maintained
coord_fixed <- function(ratio = 1, ...) {
  coord_cartesian(ratio = ratio, ...)
}

#' @export
#' @rdname coord_fixed
#' @usage NULL
coord_equal <- coord_fixed


#' @rdname Coord
#' @format NULL
#' @usage NULL
#' @export
CoordFixed <- ggproto("CoordFixed", CoordCartesian,
  is_free = function() {
    FALSE
  },

  aspect = function(self, ranges) {
    diff(ranges$y.range) / diff(ranges$x.range) * self$ratio
  }
)
