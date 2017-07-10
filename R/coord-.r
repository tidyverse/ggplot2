#' @section Coordinate systems:
#'
#' All `coord_*` functions (like `coord_trans`) return a `Coord*`
#' object (like `CoordTrans`). The `Coord*` object is responsible for
#' adjusting the position of overlapping geoms.
#'
#' The way that the `coord_*` functions work is slightly different from the
#' `geom_*` and `stat_*` functions, because a `coord_*` function
#' actually "instantiates" the `Coord*` object by creating a descendant,
#' and returns that.
#'
#' Each of the `Coord*` objects is a [ggproto()] object,
#' descended from the top-level `Coord`.  To create a new type of Coord
#' object, you typically will want to implement one or more of the following:
#'
#'   - `aspect`: Returns the desired aspect ratio for the plot.
#'   - `labels`: Returns a list containing labels for x and y.
#'   - `render_fg`: Renders foreground elements.
#'   - `render_bg`: Renders background elements.
#'   - `render_axis_h`: Renders the horizontal axes.
#'   - `render_axis_v`: Renders the vertical axes.
#'   - `range`: Returns the x and y ranges
#'   - `train`: Return the trained scale ranges.
#'   - `transform`: Transforms x and y coordinates.
#'   - `distance`: Calculates distance.
#'   - `is_linear`: Returns `TRUE` if the coordinate system is
#'     linear; `FALSE` otherwise.
#'
#'   - `setup_params(data)`: Allows the coordinate system to inspect
#'     all layers and return a list of additional parameters that vary based on
#'     the data. These parameters are currently only passed to the other
#'     setup functions and `train()`.
#'   - `setup_data(data, params)`: Allows the coordinate system to
#'     manipulate the plot data. Should return list of data frames.
#'   - `setup_layout(layout, params)`: Allows the coordinate
#'     system to manipulate the `layout` data frame which assigns
#'     data to panels and scales.
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Coord <- ggproto("Coord",

  aspect = function(ranges) NULL,

  labels = function(panel_params) panel_params,

  render_fg = function(panel_params, theme) element_render(theme, "panel.border"),

  render_bg = function(panel_params, theme) {
    x.major <- if (length(panel_params$x.major) > 0) unit(panel_params$x.major, "native")
    x.minor <- if (length(panel_params$x.minor) > 0) unit(panel_params$x.minor, "native")
    y.major <- if (length(panel_params$y.major) > 0) unit(panel_params$y.major, "native")
    y.minor <- if (length(panel_params$y.minor) > 0) unit(panel_params$y.minor, "native")

    guide_grid(theme, x.minor, x.major, y.minor, y.major)
  },

  render_axis_h = function(panel_params, theme) {
    arrange <- panel_params$x.arrange %||% c("secondary", "primary")

    list(
      top = render_axis(panel_params, arrange[1], "x", "top", theme),
      bottom = render_axis(panel_params, arrange[2], "x", "bottom", theme)
    )
  },

  render_axis_v = function(panel_params, theme) {
    arrange <- panel_params$y.arrange %||% c("primary", "secondary")

    list(
      left = render_axis(panel_params, arrange[1], "y", "left", theme),
      right = render_axis(panel_params, arrange[2], "y", "right", theme)
    )
  },

  range = function(panel_params) {
    return(list(x = panel_params$x.range, y = panel_params$y.range))
  },

  setup_panel_params = function(scale_x, scale_y, params = list()) {
    list()
  },

  transform = function(data, range) NULL,

  distance = function(x, y, panel_params) NULL,

  is_linear = function() FALSE,

  setup_params = function(data) {
    list()
  },

  setup_data = function(data, params = list()) {
    data
  },

  setup_layout = function(layout, params) {
    layout
  },

  # Optionally, modify list of x and y scales in place. Currently
  # used as a fudge for CoordFlip and CoordPolar
  modify_scales = function(scales_x, scales_y) {
    invisible()
  }
)

#' Is this object a coordinate system?
#'
#' @export is.Coord
#' @keywords internal
is.Coord <- function(x) inherits(x, "Coord")

expand_default <- function(scale, discrete = c(0, 0.6, 0, 0.6), continuous = c(0.05, 0, 0.05, 0)) {
  scale$expand %|W|% if (scale$is_discrete()) discrete else continuous
}

# Renders an axis with the correct orientation or zeroGrob if no axis should be
# generated
render_axis <- function(panel_params, axis, scale, position, theme) {
  if (axis == "primary") {
    guide_axis(panel_params[[paste0(scale, ".major")]], panel_params[[paste0(scale, ".labels")]], position, theme)
  } else if (axis == "secondary" && !is.null(panel_params[[paste0(scale, ".sec.major")]])) {
    guide_axis(panel_params[[paste0(scale, ".sec.major")]], panel_params[[paste0(scale, ".sec.labels")]], position, theme)
  } else {
    zeroGrob()
  }
}
