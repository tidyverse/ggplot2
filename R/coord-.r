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
#' @S3method coord_aspect default
coord_aspect.default <- function(coord, ranges) NULL

coord_labels <- function(coord, scales) UseMethod("coord_labels")
#' @S3method coord_labels default
coord_labels.default <- function(coord, scales) scales

coord_render_fg <- function(coord, scales, theme) 
  UseMethod("coord_render_fg")
#' @S3method coord_render_fg default
coord_render_fg.default <- function(coord, scales, theme)
  theme_render(theme, "panel.border")

coord_render_bg <- function(coord, scales, theme) 
  UseMethod("coord_render_bg")
#' @S3method coord_render_bg default
coord_render_bg.default <- function(coord, details, theme) {
  x.major <- if(length(details$x.major) > 0) unit(details$x.major, "native")
  x.minor <- if(length(details$x.minor) > 0) unit(details$x.minor, "native")
  y.major <- if(length(details$y.major) > 0) unit(details$y.major, "native")
  y.minor <- if(length(details$y.minor) > 0) unit(details$y.minor, "native")

  guide_grid(theme, x.minor, x.major, y.minor, y.major)
}

coord_render_axis_h <- function(coord, scales, theme) 
  UseMethod("coord_render_axis_h")
#' @S3method coord_render_axis_h default
coord_render_axis_h.default <- function(coord, details, theme) {
  guide_axis(details$x.major, details$x.labels, "bottom", theme)
}

coord_render_axis_v <- function(coord, scales, theme) 
  UseMethod("coord_render_axis_v")
#' @S3method coord_render_axis_v default
coord_render_axis_v.default <- function(coord, details, theme) {
  guide_axis(details$y.major, details$y.labels, "left", theme)
}

coord_range <- function(coord, scales)
  UseMethod("coord_range")

#' @S3method coord_range default
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
is.linear.default <- function(coord) FALSE

#' Set the default expand values for the scale, if NA
#' @keyword internal
coord_expand_defaults <- function(coord, scale, aesthetic)
  UseMethod("coord_expand_defaults")

#' @S3method coord_expand_defaults default
coord_expand_defaults.default <- function(coord, scale, aesthetic) {
  # Expand the same regardless of whether it's x or y
  scale$expand <- expand_default(scale, c(0, 0.6), c(0.05, 0))
  return(scale)
}

# This is a utility function used by coord_expand_defaults, to expand a single scale
expand_default <- function(scale, discrete = c(0, 0), continuous = c(0, 0)) {
  # Default expand values for discrete and continuous scales
  if (is.waive(scale$expand)) {
    if      ("discrete"   %in% class(scale))  return(discrete)
    else if ("continuous" %in% class(scale))  return(continuous)

  } else {
    return(expand)
  }
}
