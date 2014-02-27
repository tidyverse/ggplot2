#' New coordinate system.
#'
#' Internal use only.
#'
#' @param ... object fields
#' @keywords internal
#' @export
coord <- function(..., subclass = c()) {
  structure(list(...), class = c(subclass, "coord"))
}

#' Is this object a coordinate system?
#'
#' @export is.coord
#' @keywords internal
is.coord <- function(x) inherits(x, "coord")

distance <- function(., x, y, details) {
  max_dist <- dist_euclidean(details$x.range, details$y.range)
  dist_euclidean(x, y) / max_dist
}

coord_aspect <- function(coord, ranges)
  UseMethod("coord_aspect")
#' @export
coord_aspect.default <- function(coord, ranges) NULL

coord_labels <- function(coord, scales) UseMethod("coord_labels")
#' @export
coord_labels.default <- function(coord, scales) scales

coord_render_fg <- function(coord, scales, theme)
  UseMethod("coord_render_fg")
#' @export
coord_render_fg.default <- function(coord, scales, theme)
  element_render(theme, "panel.border")

coord_render_bg <- function(coord, scales, theme)
  UseMethod("coord_render_bg")
#' @export
coord_render_bg.default <- function(coord, details, theme) {
  x.major <- if(length(details$x.major) > 0) unit(details$x.major, "native")
  x.minor <- if(length(details$x.minor) > 0) unit(details$x.minor, "native")
  y.major <- if(length(details$y.major) > 0) unit(details$y.major, "native")
  y.minor <- if(length(details$y.minor) > 0) unit(details$y.minor, "native")

  guide_grid(theme, x.minor, x.major, y.minor, y.major)
}

coord_render_axis_h <- function(coord, scales, theme)
  UseMethod("coord_render_axis_h")
#' @export
coord_render_axis_h.default <- function(coord, details, theme) {
  guide_axis(details$x.major, details$x.labels, "bottom", theme)
}

coord_render_axis_v <- function(coord, scales, theme)
  UseMethod("coord_render_axis_v")
#' @export
coord_render_axis_v.default <- function(coord, details, theme) {
  guide_axis(details$y.major, details$y.labels, "left", theme)
}

coord_range <- function(coord, scales)
  UseMethod("coord_range")

#' @export
coord_range.default <- function(coord, scales) {
  return(list(x = scales$x.range, y = scales$y.range))
}

coord_train <- function(coord, scales)
  UseMethod("coord_train")

coord_transform <- function(coord, data, range)
  UseMethod("coord_transform")

coord_distance <- function(coord, x, y, details)
  UseMethod("coord_distance")

is.linear <- function(coord) UseMethod("is.linear")
#' @export
is.linear.default <- function(coord) FALSE

#' Set the default expand values for the scale, if NA
#' @keywords internal
coord_expand_defaults <- function(coord, scale, aesthetic = NULL)
  UseMethod("coord_expand_defaults")

#' @export
coord_expand_defaults.default <- function(coord, scale, aesthetic = NULL) {
  # Expand the same regardless of whether it's x or y

  # @kohske TODO:
  # Here intentionally verbose. These constants may be held by coord as, say,
  # coord$default.expand <- list(discrete = ..., continuous = ...)
  #
  # @kohske
  # Now scale itself is not changed.
  # This function only returns expanded (numeric) limits
  discrete <- c(0, 0.6)
  continuous <-  c(0.05, 0)
  expand_default(scale, discrete, continuous)
}

# This is a utility function used by coord_expand_defaults, to expand a single scale
expand_default <- function(scale, discrete = c(0, 0), continuous = c(0, 0)) {
  # Default expand values for discrete and continuous scales
  if (is.waive(scale$expand)) {
    if (inherits(scale, "discrete")) discrete
    else if (inherits(scale, "continuous")) continuous
  } else {
    return(scale$expand)
  }
}
