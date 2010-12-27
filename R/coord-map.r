CoordMap <- proto(Coord, {  
  new <- function(., projection="mercator", ..., orientation = NULL, xlim = NULL, ylim = NULL, fast = TRUE) {
    if (!fast) {
      warning("Fast parameter now ignored.  Munching always occurs", 
        call. = FALSE)
    }
    
    try_require("mapproj")
    .$proto(
      projection = projection, 
      orientation = orientation,
      xlim = xlim,
      ylim = ylim,
      params = list(...)
    )
  }
  
  muncher <- function(.) TRUE
  
  transform <- function(., data, details) {
    trans <- .$mproject(data$x, data$y, details$orientation)
    out <- cunion(trans[c("x", "y")], data)
    
    out$x <- rescale(out$x, 0:1, details$x.range)
    out$y <- rescale(out$y, 0:1, details$y.range)
    out
  }
  
  distance <- function(., x, y, details) {
    max_dist <- dist_central_angle(details$x.raw, details$y.raw)
    dist_central_angle(x, y) / max_dist
  }
  
  compute_aspect <- function(., ranges) {
    diff(ranges$y.range) / diff(ranges$x.range)
  }
  
  
  mproject <- function(., x, y, orientation) {    
    suppressWarnings(do.call("mapproject",  list(
      data.frame(x = x, y = y), 
      projection = .$projection, 
      parameters  = .$params, 
      orientation = orientation
    )))
  }

  compute_ranges <- function(., scales) {
    x.raw <- .$xlim %||% scale_dimension(scales$x)
    y.raw <- .$ylim %||% scale_dimension(scales$y)
    orientation <- .$orientation %||% c(90, 0, mean(x.raw))
    
    # Increase chances of creating valid boundary region
    grid <- expand.grid(
      x = seq(x.raw[1], x.raw[2], length = 50),
      y = seq(y.raw[1], y.raw[2], length = 50)
    )
    range <- .$mproject(grid$x, grid$y, orientation)$range
    
    x.range <- range[1:2]
    x.major <- scale_breaks(scales$x)
    x.minor <- scale_breaks_minor(scales$x)
    x.labels <- scale_labels(scales$x, x.major)

    y.range <- range[3:4]
    y.major <- scale_breaks(scales$y)
    y.minor <- scale_breaks_minor(scales$y)
    y.labels <- scale_labels(scales$y, y.major)
    
    list(
      x.raw = x.raw, y.raw = y.raw, orientation = orientation,
      x.range = x.range, y.range = y.range, 
      x.major = x.major, x.minor = x.minor, x.labels = x.labels,
      y.major = y.major, y.minor = y.minor, y.labels = y.labels
    )
  }
  
  guide_background <- function(., details, theme) {    
    xrange <- expand_range(details$x.raw, 0.2)
    yrange <- expand_range(details$y.raw, 0.2)
    xgrid <- with(details, expand.grid(
      y = c(seq(yrange[1], yrange[2], len = 50), NA),
      x = x.major
    ))
    ygrid <- with(details, expand.grid(
      x = c(seq(xrange[1], xrange[2], len = 50), NA), 
      y = y.major
    ))
    
    xlines <- .$transform(xgrid, details)
    ylines <- .$transform(ygrid, details)

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

  guide_axis_h <- function(., details, theme) {
    x_intercept <- with(details, data.frame(
      x = x.major,
      y = y.raw[1]
    ))
    pos <- .$transform(x_intercept, details)
    
    guide_axis(pos$x, details$x.labels, "bottom", theme)
  }
  guide_axis_v <- function(., details, theme) {
    x_intercept <- with(details, data.frame(
      x = x.raw[1],
      y = y.major
    ))
    pos <- .$transform(x_intercept, details)
    
    guide_axis(pos$y, details$y.labels, "left", theme)
  }


  # Documentation -----------------------------------------------

  objname <- "map"
  desc <- "Map projections"
  icon <- function(.) {
    nz <- data.frame(map("nz", plot=FALSE)[c("x","y")])
    nz$x <- nz$x - min(nz$x, na.rm=TRUE)
    nz$y <- nz$y - min(nz$y, na.rm=TRUE)
    nz <- nz / max(nz, na.rm=TRUE)
    linesGrob(nz$x, nz$y, default.units="npc")
  }
  
  desc_params <- list(
    "projection" = "projection to use, see ?mapproject for complete list",
    "..." = "other arguments passed on to mapproject",
    "orientation" = "orientation, which defaults to c(90, 0, mean(range(x))).  This is not optimal for many projections, so you will have to supply your own."
  )
  
  details <- "<p>This coordinate system provides the full range of map projections available in the mapproj package.</p>\n\n<p>This is still experimental, and if you have any advice to offer regarding a better (or more correct) way to do this, please let me know</p>\n"
  
  examples <- function(.) {
    try_require("maps")
    # Create a lat-long dataframe from the maps package
    nz <- data.frame(map("nz", plot=FALSE)[c("x","y")])
    (nzmap <- qplot(x, y, data=nz, geom="path"))
    
    nzmap + coord_map()
    nzmap + coord_map(project="cylindrical")
    nzmap + coord_map(project='azequalarea',orientation=c(-36.92,174.6,0))
    
    states <- data.frame(map("state", plot=FALSE)[c("x","y")])
    (usamap <- qplot(x, y, data=states, geom="path"))
    usamap + coord_map()
    # See ?mapproject for coordinate systems and their parameters
    usamap + coord_map(project="gilbert")
    usamap + coord_map(project="lagrange")

    # For most projections, you'll need to set the orientation yourself
    # as the automatic selection done by mapproject is not available to
    # ggplot
    usamap + coord_map(project="orthographic")
    usamap + coord_map(project="stereographic")
    usamap + coord_map(project="conic", lat0 = 30)
    usamap + coord_map(project="bonne", lat0 = 50)
  }
})
