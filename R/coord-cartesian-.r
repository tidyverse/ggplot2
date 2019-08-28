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
coord_cartesian <- function(xlim = NULL, ylim = NULL, expand = TRUE,
                            default = FALSE, clip = "on") {
  ggproto(NULL, CoordCartesian,
    limits = list(x = xlim, y = ylim),
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

  transform = function(data, panel_params) {
    data <- transform_position(data, panel_params$x$rescale, panel_params$y$rescale)
    transform_position(data, squish_infinite, squish_infinite)
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    c(
      view_scales_from_scale(scale_x, self$limits$x, self$expand),
      view_scales_from_scale(scale_y, self$limits$y, self$expand)
    )
  },

  setup_panel_guides = function(self, panel_params, guides, params = list()) {
    aesthetics <- c("x", "y", "x.sec", "y.sec")
    names(aesthetics) <- aesthetics

    # resolve the specified guide from the scale and/or guides
    guides <- lapply(aesthetics, function(aesthetic) {
      resolve_guide(
        aesthetic,
        panel_params[[aesthetic]],
        guides,
        default = guide_axis(),
        null = guide_none()
      )
    })

    # resolve the guide definition as a "guide" S3
    guides <- lapply(guides, validate_guide)

    # if there is an "position" specification in the scale, pass this on to the guide
    # ideally, this should be specified in the guide
    guides <- lapply(aesthetics, function(aesthetic) {
      guide <- guides[[aesthetic]]
      scale <- panel_params[[aesthetic]]
      # position could be NULL here for an empty scale
      guide$position <- guide$position %|W|% scale$position
      guide
    })

    panel_params$guides <- guides
    panel_params
  },

  train_panel_guides = function(self, panel_params, layers, default_mapping, params = list()) {
    aesthetics <- c("x", "y", "x.sec", "y.sec")
    names(aesthetics) <- aesthetics

    panel_params$guides <- lapply(aesthetics, function(aesthetic) {
      axis <- substr(aesthetic, 1, 1)
      guide <- panel_params$guides[[aesthetic]]
      guide <- guide_train(guide, panel_params[[aesthetic]])
      guide <- guide_transform(guide, self, panel_params)
      guide <- guide_geom(guide, layers, default_mapping)
      guide
    })

    panel_params
  },

  labels = function(self, labels, panel_params) {
    positions_x <- c("top", "bottom")
    positions_y <- c("left", "right")

    list(
      x = lapply(c(1, 2), function(i) {
        panel_guide_label(
          panel_params$guides,
          position = positions_x[[i]],
          default_label = labels$x[[i]]
        )
      }),
      y = lapply(c(1, 2), function(i) {
        panel_guide_label(
          panel_params$guides,
          position = positions_y[[i]],
          default_label = labels$y[[i]])
      })
    )
  },

  render_bg = function(panel_params, theme) {
    guide_grid(
      theme,
      panel_params$x$break_positions_minor(),
      panel_params$x$break_positions(),
      panel_params$y$break_positions_minor(),
      panel_params$y$break_positions()
    )
  },

  render_axis_h = function(panel_params, theme) {
    list(
      top = panel_guides_grob(panel_params$guides, position = "top", theme = theme),
      bottom = panel_guides_grob(panel_params$guides, position = "bottom", theme = theme)
    )
  },

  render_axis_v = function(panel_params, theme) {
    list(
      left = panel_guides_grob(panel_params$guides, position = "left", theme = theme),
      right = panel_guides_grob(panel_params$guides, position = "right", theme = theme)
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
    arrange = scale$axis_order(),
    range = continuous_range
  )
  names(view_scales) <- c(aesthetic, paste0(aesthetic, ".", names(view_scales)[-1]))

  view_scales
}

panel_guide_label <- function(guides, position, default_label) {
  guide <- guide_for_position(guides, position) %||% guide_none(title = NULL)
  guide$title %|W|% default_label
}

panel_guides_grob <- function(guides, position, theme) {
  guide <- guide_for_position(guides, position) %||% guide_none()
  guide_gengrob(guide, theme)
}

guide_for_position <- function(guides, position) {
  has_position <- vapply(
    guides,
    function(guide) identical(guide$position, position),
    logical(1)
  )

  guides <- guides[has_position]
  guides_order <- vapply(guides, function(guide) as.numeric(guide$order)[1], numeric(1))
  Reduce(guide_merge, guides[order(guides_order)])
}
