#' @section Coordinate systems:
#'
#' All \code{coord_*} functions (like \code{coord_trans}) return a \code{Coord*}
#' object (like \code{CoordTrans}). The \code{Coord*} object is responsible for
#' adjusting the position of overlapping geoms.
#'
#' The way that the \code{coord_*} functions work is slightly different from the
#' \code{geom_*} and \code{stat_*} functions, because a \code{coord_*} function
#' actually "instantiates" the \code{Coord*} object by creating a descendant,
#' and returns that.
#'
#' Each of the \code{Coord*} objects is a \code{\link{ggproto}} object,
#' descended from the top-level \code{Coord}.  To create a new type of Coord
#' object, you typically will want to implement one or more of the following:
#'
#' \itemize{
#'   \item \code{aspect}: Returns the desired aspect ratio for the plot.
#'   \item \code{labels}: Returns a list containing labels for x and y.
#'   \item \code{render_fg}: Renders foreground elements.
#'   \item \code{render_bg}: Renders background elements.
#'   \item \code{render_axis_h}: Renders the horizontal axis.
#'   \item \code{render_axis_v}: Renders the vertical axis.
#'   \item \code{range}: Returns the x and y ranges
#'   \item \code{train}: Return the trained scale ranges.
#'   \item \code{transform}: Transforms x and y coordinates.
#'   \item \code{distance}: Calculates distance.
#'   \item \code{is_linear}: Returns \code{TRUE} if the coordinate system is
#'     linear; \code{FALSE} otherwise.
#' }
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Coord <- ggproto("Coord",

  aspect = function(ranges) NULL,

  labels = function(scale_details) scale_details,

  render_fg = function(scale_details, theme) element_render(theme, "panel.border"),

  render_bg = function(scale_details, theme) {
    x.major <- if (length(scale_details$x.major) > 0) unit(scale_details$x.major, "native")
    x.minor <- if (length(scale_details$x.minor) > 0) unit(scale_details$x.minor, "native")
    y.major <- if (length(scale_details$y.major) > 0) unit(scale_details$y.major, "native")
    y.minor <- if (length(scale_details$y.minor) > 0) unit(scale_details$y.minor, "native")

    guide_grid(theme, x.minor, x.major, y.minor, y.major)
  },

  render_axis_h = function(scale_details, theme) {
    guide_axis(scale_details$x.major, scale_details$x.labels, "bottom", theme)
  },

  render_axis_v = function(scale_details, theme) {
    guide_axis(scale_details$y.major, scale_details$y.labels, "left", theme)
  },

  range = function(scale_details) {
    return(list(x = scale_details$x.range, y = scale_details$y.range))
  },

  train = function(scale_details) NULL,

  transform = function(data, range) NULL,

  distance = function(x, y, scale_details) NULL,

  is_linear = function() FALSE
)

#' Is this object a coordinate system?
#'
#' @export is.Coord
#' @keywords internal
is.Coord <- function(x) inherits(x, "Coord")

expand_default <- function(scale, discrete = c(0, 0.6), continuous = c(0.05, 0)) {
  scale$expand %|W|% if (scale$is_discrete()) discrete else continuous
}
