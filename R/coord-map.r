#' Map projections.
#'
#' This coordinate system provides the full range of map projections available
#' in the mapproj package.
#'
#' This is still experimental, and if you have any advice to offer regarding
#' a better (or more correct) way to do this, please let me know
#'
#' @export
#' @param projection projection to use, see
#'    \code{\link[mapproj]{mapproject}} for list
#' @param ... other arguments passed on to
#'   \code{\link[mapproj]{mapproject}}
#' @param orientation projection orientation, which defaults to
#'  \code{c(90, 0, mean(range(x)))}.  This is not optimal for many
#'  projections, so you will have to supply your own. See
#'  \code{\link[mapproj]{mapproject}} for more information.
#' @param xlim manually specific x limits (in degrees of lontitude)
#' @param ylim manually specific y limits (in degrees of latitude)
#' @export
#' @examples
#' if (require("maps")) {
#' # Create a lat-long dataframe from the maps package
#' nz <- map_data("nz")
#' nzmap <- ggplot(nz, aes(x=long, y=lat, group=group)) +
#'   geom_polygon(fill="white", colour="black")
#'
#' # Use cartesian coordinates
#' nzmap
#' # With default mercator projection
#' nzmap + coord_map()
#' # Other projections
#' nzmap + coord_map("cylindrical")
#' nzmap + coord_map("azequalarea",orientation=c(-36.92,174.6,0))
#'
#' states <- map_data("state")
#' usamap <- ggplot(states, aes(x=long, y=lat, group=group)) +
#'   geom_polygon(fill="white", colour="black")
#'
#' # Use cartesian coordinates
#' usamap
#' # With mercator projection
#' usamap + coord_map()
#' # See ?mapproject for coordinate systems and their parameters
#' usamap + coord_map("gilbert")
#' usamap + coord_map("lagrange")
#'
#' # For most projections, you'll need to set the orientation yourself
#' # as the automatic selection done by mapproject is not available to
#' # ggplot
#' usamap + coord_map("orthographic")
#' usamap + coord_map("stereographic")
#' usamap + coord_map("conic", lat0 = 30)
#' usamap + coord_map("bonne", lat0 = 50)
#'
#' # World map, using geom_path instead of geom_polygon
#' world <- map_data("world")
#' worldmap <- ggplot(world, aes(x=long, y=lat, group=group)) +
#'   geom_path() +
#'   scale_y_continuous(breaks=(-2:2) * 30) +
#'   scale_x_continuous(breaks=(-4:4) * 45)
#'
#' # Orthographic projection with default orientation (looking down at North pole)
#' worldmap + coord_map("ortho")
#' # Looking up up at South Pole
#' worldmap + coord_map("ortho", orientation=c(-90, 0, 0))
#' # Centered on New York (currently has issues with closing polygons)
#' worldmap + coord_map("ortho", orientation=c(41, -74, 0))
#' }
coord_map <- function(projection="mercator", ..., orientation = NULL, xlim = NULL, ylim = NULL) {
  try_require("mapproj")
  coord(
    projection = projection,
    orientation = orientation,
    limits = list(x = xlim, y = ylim),
    params = list(...),
    subclass = "map"
  )
}

#' @export
coord_transform.map <- function(coord, data, details) {
  trans <- mproject(coord, data$x, data$y, details$orientation)
  out <- cunion(trans[c("x", "y")], data)

  out$x <- rescale(out$x, 0:1, details$x.proj)
  out$y <- rescale(out$y, 0:1, details$y.proj)
  out
}
mproject <- function(coord, x, y, orientation) {
  suppressWarnings(mapproj::mapproject(x, y,
    projection = coord$projection,
    parameters  = coord$params,
    orientation = orientation
  ))
}

#' @export
coord_distance.map <- function(coord, x, y, details) {
  max_dist <- dist_central_angle(details$x.range, details$y.range)
  dist_central_angle(x, y) / max_dist
}

#' @export
coord_aspect.map <- function(coord, ranges) {
  diff(ranges$y.proj) / diff(ranges$x.proj)
}

#' @export
coord_train.map <- function(coord, scales) {

  # range in scale
  ranges <- list()
  for (n in c("x", "y")) {

    scale <- scales[[n]]
    limits <- coord$limits[[n]]

    if (is.null(limits)) {
      expand <- coord_expand_defaults(coord, scale, n)
      range <- scale_dimension(scale, expand)
    } else {
      range <- range(scale_transform(scale, limits))
    }
    ranges[[n]] <- range
  }

  orientation <- coord$orientation %||% c(90, 0, mean(ranges$x))

  # Increase chances of creating valid boundary region
  grid <- expand.grid(
    x = seq(ranges$x[1], ranges$x[2], length = 50),
    y = seq(ranges$y[1], ranges$y[2], length = 50)
  )

  ret <- list(x = list(), y = list())

  # range in map
  proj <- mproject(coord, grid$x, grid$y, orientation)$range
  ret$x$proj <- proj[1:2]
  ret$y$proj <- proj[3:4]

  for (n in c("x", "y")) {
    out <- scale_break_info(scales[[n]], ranges[[n]])
    ret[[n]]$range <- out$range
    ret[[n]]$major <- out$major_source
    ret[[n]]$minor <- out$minor_source
    ret[[n]]$labels <- out$labels
  }

  details <- list(
    orientation = orientation,
    x.range = ret$x$range, y.range = ret$y$range,
    x.proj = ret$x$proj, y.proj = ret$y$proj,
    x.major = ret$x$major, x.minor = ret$x$minor, x.labels = ret$x$labels,
    y.major = ret$y$major, y.minor = ret$y$minor, y.labels = ret$y$labels
  )
  details
}

#' @export
coord_render_bg.map <- function(coord, details, theme) {
  xrange <- expand_range(details$x.range, 0.2)
  yrange <- expand_range(details$y.range, 0.2)

  # Limit ranges so that lines don't wrap around globe
  xmid <- mean(xrange)
  ymid <- mean(yrange)
  xrange[xrange < xmid - 180] <- xmid - 180
  xrange[xrange > xmid + 180] <- xmid + 180
  yrange[yrange < ymid - 90] <- ymid - 90
  yrange[yrange > ymid + 90] <- ymid + 90

  xgrid <- with(details, expand.grid(
    y = c(seq(yrange[1], yrange[2], len = 50), NA),
    x = x.major
  ))
  ygrid <- with(details, expand.grid(
    x = c(seq(xrange[1], xrange[2], len = 50), NA),
    y = y.major
  ))

  xlines <- coord_transform(coord, xgrid, details)
  ylines <- coord_transform(coord, ygrid, details)

  if (nrow(xlines) > 0) {
    grob.xlines <- element_render(
      theme, "panel.grid.major.x",
      xlines$x, xlines$y, default.units = "native"
    )
  } else {
    grob.xlines <- zeroGrob()
  }

  if (nrow(ylines) > 0) {
    grob.ylines <- element_render(
      theme, "panel.grid.major.y",
      ylines$x, ylines$y, default.units = "native"
    )
  } else {
    grob.ylines <- zeroGrob()
  }

  ggname("grill", grobTree(
    element_render(theme, "panel.background"),
    grob.xlines, grob.ylines
  ))
}

#' @export
coord_render_axis_h.map <- function(coord, details, theme) {
  if (is.null(details$x.major)) return(zeroGrob())

  x_intercept <- with(details, data.frame(
    x = x.major,
    y = y.range[1]
  ))
  pos <- coord_transform(coord, x_intercept, details)

  guide_axis(pos$x, details$x.labels, "bottom", theme)
}
#' @export
coord_render_axis_v.map <- function(coord, details, theme) {
  if (is.null(details$y.major)) return(zeroGrob())

  x_intercept <- with(details, data.frame(
    x = x.range[1],
    y = y.major
  ))
  pos <- coord_transform(coord, x_intercept, details)

  guide_axis(pos$y, details$y.labels, "left", theme)
}
