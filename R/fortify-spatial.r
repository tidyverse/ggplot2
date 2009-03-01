fortify.SpatialPolygonsDataFrame <- function(shape, region = NULL) {
  attr <- as.data.frame(shape)
  # If not specified, split into regions based on first variable in attributes
  if (is.null(region)) {
    region <- names(attr)[1]
    message("Using ", region, " to define regions.")
  }
  
  # Figure out how polygons should be split up into the region of interest
  polys <- split(as.numeric(row.names(attr)), list(attr[, region]))
  cp <- polygons(shape)
  
  # Union together all polygons that make up a region
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
