#' Cartesian coordinates
#'
#' The Cartesian coordinate system is the most familiar, and common, type of
#' coordinate system. Setting limits on the coordinate system will zoom the
#' plot (like you're looking at it with a magnifying glass), and will not
#' change the underlying data like setting limits on a scale will.
#'
#' @param xlim,ylim Limits for the x and y axes.
#' @param expand If `TRUE`, the default, adds a small expansion factor to
#'   the limits to ensure that data and axes don't overlap. If `FALSE`,
#'   limits are taken exactly from the data or `xlim`/`ylim`.
#' @export
#' @examples
#' # There are two ways of zooming the plot display: with scales or
#' # with coordinate systems.  They work in two rather different ways.
#'
#' p <- ggplot(mtcars, aes(disp, wt)) +
#'   geom_point() +
#'   geom_smooth()
#' p
#'
#' # Setting the limits on a scale converts all values outside the range to NA.
#' p + scale_x_continuous(limits = c(325, 500))
#'
#' # Setting the limits on the coordinate system performs a visual zoom.
#' # The data is unchanged, and we just view a small portion of the original
#' # plot. Note how smooth continues past the points visible on this plot.
#' p + coord_cartesian(xlim = c(325, 500))
#'
#' # By default, the same expansion factor is applied as when setting scale
#' # limits. You can set the limits precisely by setting expand = FALSE
#' p + coord_cartesian(xlim = c(325, 500), expand = FALSE)
#'
#' # Simiarly, we can use expand = FALSE to turn off expansion with the
#' # default limits
#' p + coord_cartesian(expand = FALSE)
#'
#' # You can see the same thing with this 2d histogram
#' d <- ggplot(diamonds, aes(carat, price)) +
#'   stat_bin2d(bins = 25, colour = "white")
#' d
#'
#' # When zooming the scale, the we get 25 new bins that are the same
#' # size on the plot, but represent smaller regions of the data space
#' d + scale_x_continuous(limits = c(0, 1))
#'
#' # When zooming the coordinate system, we see a subset of original 50 bins,
#' # displayed bigger
#' d + coord_cartesian(xlim = c(0, 1))
coord_cartesian <- function(xlim = NULL, ylim = NULL, expand = TRUE) {
  ggproto(NULL, CoordCartesian,
    limits = list(x = xlim, y = ylim),
    expand = expand
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordCartesian <- ggproto("CoordCartesian", Coord,

  is_linear = function() TRUE,

  distance = function(x, y, panel_params) {
    max_dist <- dist_euclidean(panel_params$x.range, panel_params$y.range)
    dist_euclidean(x, y) / max_dist
  },

  transform = function(data, panel_params) {
    rescale_x <- function(data) rescale(data, from = panel_params$x.range)
    rescale_y <- function(data) rescale(data, from = panel_params$y.range)

    data <- transform_position(data, rescale_x, rescale_y)
    transform_position(data, squish_infinite, squish_infinite)
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    train_cartesian <- function(scale, limits, name) {
      range <- scale_range(scale, limits, self$expand)

      out <- scale$break_info(range)
      out$arrange <- scale$axis_order()
      names(out) <- paste(name, names(out), sep = ".")
      out
    }

    c(
      train_cartesian(scale_x, self$limits$x, "x"),
      train_cartesian(scale_y, self$limits$y, "y")
    )
  }
)

scale_range <- function(scale, limits = NULL, expand = TRUE) {
  expansion <- if (expand) expand_default(scale) else c(0, 0)

  if (is.null(limits)) {
    scale$dimension(expansion)
  } else {
    range <- range(scale$transform(limits))
    expand_range(range, expansion[1], expansion[2])
  }
}
