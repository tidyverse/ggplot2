#' Fortify method for classes from the sp package.
#' 
#' To figure out the correct variable name for region, inspect 
#' \code{as.data.frame(model)}.
#' 
#' @S3method fortify SpatialPolygons
#' @S3method fortify SpatialPolygonsDataFrame
#' @S3method fortify Polygons
#' @S3method fortify Polygon
#' @S3method fortify SpatialLinesDataFrame
#' @S3method fortify Lines
#' @S3method fortify Line
#' @param model \code{SpatialPolygonsDataFrame} to convert into a dataframe.
#' @param data not used by this method
#' @param region name of variable used to split up regions
#' @param ... not used by this method
#' @name fortify.sp
NULL

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
  ldply(model@polygons, fortify)
}

fortify.Polygons <- function(model, data, ...) {
  subpolys <- model@Polygons
  pieces <- ldply(seq_along(subpolys), function(i) {
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
  ldply(model@lines, fortify)
}

fortify.Lines <- function(model, data, ...) {
  lines <- model@Lines
  pieces <- ldply(seq_along(lines), function(i) {
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
