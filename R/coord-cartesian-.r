#' Cartesian coordinates.
#'
#' The Cartesian coordinate system is the most familiar, and common, type of
#' coordinate system. Setting limits on the coordinate system will zoom the
#' plot (like you're looking at it with a magnifying glass), and will not
#' change the underlying data like setting limits on a scale will.
#'
#' @param xlim limits for the x axis
#' @param ylim limits for the y axis
#' @param xexpand a numeric vector of length two giving multiplicative
#'   and additive expansion constants. These constants ensure that the
#'   data is placed some distance away from the x axis.
#' @param yexpand same as xexpand, but for the y axis
#' @export
#' @examples
#' # There are two ways of zooming the plot display: with scales or
#' # with coordinate systems.  They work in two rather different ways.
#'
#' (p <- ggplot(mtcars, aes(disp, wt)) +
#'   geom_smooth())
#'
#' # Setting the limits on a scale will throw away all data that's not
#' # inside these limits.  This is equivalent to plotting a subset of
#' # the original data
#' p + scale_x_continuous(limits = c(325, 500))
#'
#' # Setting the limits on the coordinate system performs a visual zoom
#' # the data is unchanged, and we just view a small portion of the original
#' # plot.  See how the axis labels are the same as the original data, and
#' # the smooth continue past the points visible on this plot.
#' p + coord_cartesian(xlim = c(325, 500))
#'
#' # You can see the same thing with this 2d histogram
#' (d <- ggplot(diamonds, aes(carat, price)) +
#'   stat_bin2d(bins = 25, colour = "grey50"))
#'
#' # When zooming the scale, we get 25 new bins that are the same
#' # size on the plot, but represent smaller regions of the data space
#' d + scale_x_continuous(limits = c(0, 2))
#'
#' # When zooming the coordinate system, we see a subset of original 50 bins,
#' # displayed bigger
#' d + coord_cartesian(xlim = c(0, 2))
coord_cartesian <- function(xlim = NULL, ylim = NULL, xexpand = waiver(), yexpand = waiver()) {
  coord(
    x = list(limits = xlim, expand = xexpand),
    y = list(limits = ylim, expand = yexpand),
    subclass = "cartesian"
  )
}

#' @export
is.linear.cartesian <- function(coord) TRUE

#' @export
coord_distance.cartesian <- function(coord, x, y, details) {
  max_dist <- dist_euclidean(details$x.range, details$y.range)
  dist_euclidean(x, y) / max_dist
}

#' @export
coord_transform.cartesian <- function(., data, details) {
  rescale_x <- function(data) rescale(data, from = details$x.range)
  rescale_y <- function(data) rescale(data, from = details$y.range)

  data <- transform_position(data, rescale_x, rescale_y)
  transform_position(data, squish_infinite, squish_infinite)
}

#' @export
coord_train.cartesian <- function(coord, scales) {
  c(train_cartesian(scales$x, coord$x, "x"),
    train_cartesian(scales$y, coord$y, "y"))
}

train_cartesian <- function(scale, .coord, name) {
  # first, calculate the range that is the numerical limits in data space

  # range defined by scale OR coord
  expand <- coord_expand_defaults(.coord, scale)
  if (is.null(.coord$limits)) {
    range <- scale_dimension(scale, expand$scale)
  } else {
    expanded_limits <- coord_dimension(scale, .coord, expand$coord)
    range <- range(scale_transform(scale, expanded_limits))
  }

  out <- scale_break_info(scale, range)
  names(out) <- paste(name, names(out), sep = ".")
  out
}
