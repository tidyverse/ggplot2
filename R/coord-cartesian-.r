#' Cartesian coordinates.
#'
#' The Cartesian coordinate system is the most familiar, and common, type of
#' coordinate system. Setting limits on the coordinate system will zoom the
#' plot (like you're looking at it with a magnifying glass), and will not
#' change the underlying data like setting limits on a scale will.
#'
#' @param xlim limits for the x axis
#' @param ylim limits for the y axis
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
#' # When zooming the scale, the we get 25 new bins that are the same
#' # size on the plot, but represent smaller regions of the data space
#' d + scale_x_continuous(limits = c(0, 2))
#'
#' # When zooming the coordinate system, we see a subset of original 50 bins,
#' # displayed bigger
#' d + coord_cartesian(xlim = c(0, 2))
coord_cartesian <- function(xlim = NULL, ylim = NULL) {
  ggproto(NULL, CoordCartesian,
    limits = list(x = xlim, y = ylim)
  )
}


CoordCartesian <- ggproto("CoordCartesian", Coord,

  is_linear = function() TRUE,

  distance = function(x, y, scale_details) {
    max_dist <- dist_euclidean(scale_details$x.range, scale_details$y.range)
    dist_euclidean(x, y) / max_dist
  },

  transform = function(data, scale_details) {
    rescale_x <- function(data) rescale(data, from = scale_details$x.range)
    rescale_y <- function(data) rescale(data, from = scale_details$y.range)

    data <- transform_position(data, rescale_x, rescale_y)
    transform_position(data, squish_infinite, squish_infinite)
  },

  train = function(self, scale_details) {
    c(train_cartesian(scale_details$x, self$limits$x, "x"),
      train_cartesian(scale_details$y, self$limits$y, "y"))
  }
)


train_cartesian <- function(scale_details, limits, name) {

  # first, calculate the range that is the numerical limits in data space

  # expand defined by scale OR coord
  if (is.null(limits)) {
    # TODO: This is weird, accessing Coord directly for this method.
    expand <- Coord$expand_defaults(scale_details)
    range <- scale_dimension(scale_details, expand)
  } else {
    range <- range(scale_transform(scale_details, limits))
  }

  out <- scale_break_info(scale_details, range)
  names(out) <- paste(name, names(out), sep = ".")
  out
}
