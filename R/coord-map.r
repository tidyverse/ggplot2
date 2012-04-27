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
#'  projections, so you will have to supply your own.
#' @param xlim manually specific x limits (in degrees of lontitude)
#' @param ylim manually specific y limits (in degrees of latitude)
#' @export
#' @examples
#' if (require("maps")) {
#' # Create a lat-long dataframe from the maps package
#' nz <- data.frame(map("nz", plot=FALSE)[c("x","y")])
#' (nzmap <- qplot(x, y, data=nz, geom="path"))
#' 
#' nzmap + coord_map()
#' nzmap + coord_map(project="cylindrical")
#' nzmap + coord_map(project='azequalarea',orientation=c(-36.92,174.6,0))
#' 
#' states <- data.frame(map("state", plot=FALSE)[c("x","y")])
#' (usamap <- qplot(x, y, data=states, geom="path"))
#' usamap + coord_map()
#' # See ?mapproject for coordinate systems and their parameters
#' usamap + coord_map(project="gilbert")
#' usamap + coord_map(project="lagrange")
#'
#' # For most projections, you'll need to set the orientation yourself
#' # as the automatic selection done by mapproject is not available to
#' # ggplot
#' usamap + coord_map(project="orthographic")
#' usamap + coord_map(project="stereographic")
#' usamap + coord_map(project="conic", lat0 = 30)
#' usamap + coord_map(project="bonne", lat0 = 50)
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

#' @S3method coord_transform map
coord_transform.map <- function(coord, data, details) {
  trans <- mproject(coord, data$x, data$y, details$orientation)
  out <- cunion(trans[c("x", "y")], data)
  
  out$x <- rescale(out$x, 0:1, details$x.proj)
  out$y <- rescale(out$y, 0:1, details$y.proj)
  out
}
mproject <- function(coord, x, y, orientation) {    
  suppressWarnings(mapproject(x, y,
    projection = coord$projection, 
    parameters  = coord$params, 
    orientation = orientation
  ))
}

#' @S3method coord_distance map
coord_distance.map <- function(coord, x, y, details) {
  max_dist <- dist_central_angle(details$x.range, details$y.range)
  dist_central_angle(x, y) / max_dist
}

#' @S3method coord_aspect map
coord_aspect.map <- function(coord, ranges) {
  diff(ranges$y.proj) / diff(ranges$x.proj)
}

#' @S3method coord_train map
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

#' @S3method coord_render_bg map
coord_render_bg.map <- function(coord, details, theme) {    
  xrange <- expand_range(details$x.range, 0.2)
  yrange <- expand_range(details$y.range, 0.2)
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

  ggname("grill", grobTree(
    theme_render(theme, "panel.background"),
    theme_render(
      theme, "panel.grid.major", name = "x", 
      xlines$x, xlines$y, default.units = "native"
    ),
    theme_render(
      theme, "panel.grid.major", name = "y", 
      ylines$x, ylines$y, default.units = "native"
    )
  ))
}  

#' @S3method coord_render_axis_h map
coord_render_axis_h.map <- function(coord, details, theme) {
  x_intercept <- with(details, data.frame(
    x = x.major,
    y = y.range[1]
  ))
  pos <- coord_transform(coord, x_intercept, details)
  
  guide_axis(pos$x, details$x.labels, "bottom", theme)
}
#' @S3method coord_render_axis_v map
coord_render_axis_v.map <- function(coord, details, theme) {
  x_intercept <- with(details, data.frame(
    x = x.range[1],
    y = y.major
  ))
  pos <- coord_transform(coord, x_intercept, details)
  
  guide_axis(pos$y, details$y.labels, "left", theme)
}

icon.map <- function(.) {
  nz <- data.frame(map("nz", plot=FALSE)[c("x","y")])
  nz$x <- nz$x - min(nz$x, na.rm=TRUE)
  nz$y <- nz$y - min(nz$y, na.rm=TRUE)
  nz <- nz / max(nz, na.rm=TRUE)
  linesGrob(nz$x, nz$y, default.units="npc")
}
