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
#'   Giving a logical vector will separately control the expansion for the four
#'   directions (top, left, bottom and right). The `expand` argument will be
#'   recycled to length 4 if necessary. Alternatively, can be a named logical
#'   vector to control a single direction, e.g. `expand = c(bottom = FALSE)`.
#' @param default Is this the default coordinate system? If `FALSE` (the default),
#'   then replacing this coordinate system with another one creates a message alerting
#'   the user that the coordinate system is being replaced. If `TRUE`, that warning
#'   is suppressed.
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#'   setting of `"on"` (the default) means yes, and a setting of `"off"`
#'   means no. In most cases, the default of `"on"` should not be changed,
#'   as setting `clip = "off"` can cause unexpected results. It allows
#'   drawing of data points anywhere on the plot, including in the plot margins. If
#'   limits are set via `xlim` and `ylim` and some data points fall outside those
#'   limits, then those data points may show up in places such as the axes, the
#'   legend, the plot title, or the plot margins.
#' @param reverse A string giving which directions to reverse. `"none"`
#'   (default) keeps directions as is. `"x"` and `"y"` can be used to reverse
#'   their respective directions. `"xy"` can be used to reverse both
#'   directions.
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
#' # Similarly, we can use expand = FALSE to turn off expansion with the
#' # default limits
#' p + coord_cartesian(expand = FALSE)
#'
#' # You can see the same thing with this 2d histogram
#' d <- ggplot(diamonds, aes(carat, price)) +
#'   stat_bin_2d(bins = 25, colour = "white")
#' d
#'
#' # When zooming the scale, the we get 25 new bins that are the same
#' # size on the plot, but represent smaller regions of the data space
#' d + scale_x_continuous(limits = c(0, 1))
#'
#' # When zooming the coordinate system, we see a subset of original 50 bins,
#' # displayed bigger
#' d + coord_cartesian(xlim = c(0, 1))
coord_cartesian <- function(xlim = NULL, ylim = NULL, expand = TRUE,
                            default = FALSE, clip = "on", reverse = "none") {
  check_coord_limits(xlim)
  check_coord_limits(ylim)
  ggproto(NULL, CoordCartesian,
    limits = list(x = xlim, y = ylim),
    reverse = reverse,
    expand = expand,
    default = default,
    clip = clip
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordCartesian <- ggproto("CoordCartesian", Coord,

  is_linear = function() TRUE,
  is_free = function() TRUE,

  distance = function(x, y, panel_params) {
    max_dist <- dist_euclidean(panel_params$x$dimension(), panel_params$y$dimension())
    dist_euclidean(x, y) / max_dist
  },

  range = function(panel_params) {
    list(x = panel_params$x$dimension(), y = panel_params$y$dimension())
  },

  backtransform_range = function(self, panel_params) {
    self$range(panel_params)
  },

  transform = function(self, data, panel_params) {
    reverse <- self$reverse %||% "none"
    x <- panel_params$x[[switch(reverse, xy = , x = "reverse", "rescale")]]
    y <- panel_params$y[[switch(reverse, xy = , y = "reverse", "rescale")]]
    data <- transform_position(data, x, y)
    transform_position(data, squish_infinite, squish_infinite)
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    c(
      view_scales_from_scale(scale_x, self$limits$x, params$expand[c(4, 2)]),
      view_scales_from_scale(scale_y, self$limits$y, params$expand[c(3, 1)])
    )
  },

  render_bg = function(self, panel_params, theme) {
    guide_grid(theme, panel_params, self)
  },

  render_axis_h = function(panel_params, theme) {
    list(
      top = panel_guides_grob(
        panel_params$guides, position = "top",
        theme = theme, labels = panel_params$draw_labels$top
      ),
      bottom = panel_guides_grob(
        panel_params$guides, position = "bottom",
        theme = theme, labels = panel_params$draw_labels$bottom
      )
    )
  },

  render_axis_v = function(panel_params, theme) {
    list(
      left = panel_guides_grob(
        panel_params$guides, position = "left",
        theme = theme, labels = panel_params$draw_labels$left
        ),
      right = panel_guides_grob(
        panel_params$guides, position = "right",
        theme = theme, labels = panel_params$draw_labels$right
      )
    )
  }
)

view_scales_from_scale <- function(scale, coord_limits = NULL, expand = TRUE) {
  expansion <- default_expansion(scale, expand = expand)
  limits <- scale$get_limits()
  continuous_range <- expand_limits_scale(scale, expansion, limits, coord_limits = coord_limits)
  aesthetic <- scale$aesthetics[1]

  view_scales <- list(
    view_scale_primary(scale, limits, continuous_range),
    sec = view_scale_secondary(scale, limits, continuous_range),
    range = continuous_range
  )
  names(view_scales) <- c(aesthetic, paste0(aesthetic, ".", names(view_scales)[-1]))

  view_scales
}

panel_guides_grob <- function(guides, position, theme, labels = NULL) {
  if (!is_guides(guides)) {
    return(zeroGrob())
  }
  pair <- guides$get_position(position)
  pair$params$draw_label <- labels %||% NULL
  pair$guide$draw(theme, params = pair$params)
}
