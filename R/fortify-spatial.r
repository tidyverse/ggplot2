# Fortify spatial polygons and lines
# Fortify method for a number of the class from the sp package.
# 
# To figure out the correct variable name for region, inspect 
# \code{as.data.frame(model)}.
# 
# @alias fortify.SpatialPolygons
# @alias fortify.Polygons
# @alias fortify.Polygon
# @alias fortify.SpatialLinesDataFrame
# @alias fortify.Lines
# @alias fortify.Line
# @arguments SpatialPolygonsDataFrame
# @arguments not used
# @arguments name of variable to split up regions by
# @arguments not used
fortify.SpatialPolygonsDataFrame <- function(model, data, region = NULL, ...) {
  attr <- as.data.frame(model)
  # If not specified, split into regions based on first variable in attributes
  if (is.null(region)) {
    region <- names(attr)[1]
    message("Using ", region, " to define regions.")
  }
  
  # Figure out how polygons should be split up into the region of interest
  
  polys <- split(as.numeric(row.names(attr)), addNA(attr[, region], TRUE))
  cp <- polygons(model)
  
  # Union together all polygons that make up a region
  try_require(c("gpclib", "maptools"))
  unioned <- unionSpatialPolygons(cp, invert(polys))
  
  coords <- fortify(unioned)
  coords$order <- 1:nrow(coords)
  coords
}

fortify.SpatialPolygons <- function(model, data, ...) {
  plyr::ldply(model@polygons, fortify)
}

fortify.Polygons <- function(model, data, ...) {
  subpolys <- model@Polygons
  pieces <- plyr::ldply(seq_along(subpolys), function(i) {
    df <- fortify(subpolys[[model@plotOrder[i]]])
    df$piece <- i
    df
  })
  
  within(pieces,{
    order <- 1:nrow(pieces)
    id <- model@ID
    piece <- factor(piece)
    group <- interaction(id, piece)
  })
}

fortify.Polygon <- function(model, data, ...) {
  df <- as.data.frame(model@coords)
  names(df) <- c("long", "lat")
  df$order <- 1:nrow(df)
  df$hole <- model@hole
  df
}

fortify.SpatialLinesDataFrame <- function(model, data, ...) {
  plyr::ldply(model@lines, fortify)
}

fortify.Lines <- function(model, data, ...) {
  lines <- model@Lines
  pieces <- plyr::ldply(seq_along(lines), function(i) {
    df <- fortify(lines[[i]])
    df$piece <- i
    df
  })
  
  within(pieces,{
    order <- 1:nrow(pieces)
    id <- model@ID
    piece <- factor(piece)
    group <- interaction(id, piece)
  })
}

fortify.Line <- function(model, data, ...) {
  df <- as.data.frame(model@coords)
  names(df) <- c("long", "lat")
  df$order <- 1:nrow(df)
  df  
}
