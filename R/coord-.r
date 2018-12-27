#' @section Coordinate systems:
#'
#' All `coord_*` functions (like `coord_trans`) return a `Coord*`
#' object (like `CoordTrans`).
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
#'   - `backtransform_range(panel_params)`: Extracts the panel range provided
#'     in `panel_params` (created by `setup_panel_params()`, see below) and
#'     back-transforms to data coordinates. This back-transformation can be needed
#'     for coords such as `coord_trans()` where the range in the transformed
#'     coordinates differs from the range in the untransformed coordinates. Returns
#'     a list of two ranges, `x` and `y`, and these correspond to the variables
#'     mapped to the `x` and `y` aesthetics, even for coords such as `coord_flip()`
#'     where the `x` aesthetic is shown along the y direction and vice versa.
#'   - `range(panel_params)`: Extracts the panel range provided
#'     in `panel_params` (created by `setup_panel_params()`, see below) and
#'     returns it. Unlike `backtransform_range()`, this function does not perform
#'     any back-transformation and instead returns final transformed coordinates. Returns
#'     a list of two ranges, `x` and `y`, and these correspond to the variables
#'     mapped to the `x` and `y` aesthetics, even for coords such as `coord_flip()`
#'     where the `x` aesthetic is shown along the y direction and vice versa.
#'   - `transform`: Transforms x and y coordinates.
#'   - `distance`: Calculates distance.
#'   - `is_linear`: Returns `TRUE` if the coordinate system is
#'     linear; `FALSE` otherwise.
#'   - `is_free`: Returns `TRUE` if the coordinate system supports free
#'     positional scales; `FALSE` otherwise.
#'   - `setup_panel_params(scale_x, scale_y, params)`: Determines the appropriate
#'     x and y ranges for each panel, and also calculates anything else needed to
#'     render the panel and axes, such as tick positions and labels for major
#'     and minor ticks. Returns all this information in a named list.
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

  # Is this the default coordinate system?
  default = FALSE,

  # should drawing be clipped to the extent of the plot panel?
  # "on" = yes, "off" = no
  clip = "on",

  aspect = function(ranges) NULL,

  labels = function(panel_params) panel_params,

  render_fg = function(panel_params, theme) element_render(theme, "panel.border"),

  render_bg = function(panel_params, theme) {
    guide_grid(theme,
               panel_params$x.minor,
               panel_params$x.major,
               panel_params$y.minor,
               panel_params$y.major)
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

  # transform range given in transformed coordinates
  # back into range in given in (possibly scale-transformed)
  # data coordinates
  backtransform_range = function(self, panel_params) {
    warning(
      "range backtransformation not implemented in this coord; results may be wrong.",
      call. = FALSE
      )
    # return result from range function for backwards compatibility
    # before ggplot2 3.0.1
    self$range(panel_params)
  },

  # return range stored in panel_params
  range = function(panel_params) {
    warning(
      "range calculation not implemented in this coord; results may be wrong.",
      call. = FALSE
    )
    list(x = panel_params$x.range, y = panel_params$y.range)
  },

  setup_panel_params = function(scale_x, scale_y, params = list()) {
    list()
  },

  transform = function(data, range) NULL,

  distance = function(x, y, panel_params) NULL,

  is_linear = function() FALSE,

  # Does the coordinate system support free scaling of axes in a faceted plot?
  # Will generally have to return FALSE for coordinate systems that enforce a fixed aspect ratio.
  is_free = function() FALSE,

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
