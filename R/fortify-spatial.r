#' Fortify method for classes from the sp package.
#'
#' To figure out the correct variable name for region, inspect
#' \code{as.data.frame(model)}.
#'
#' @param model \code{SpatialPolygonsDataFrame} to convert into a dataframe.
#' @param data not used by this method
#' @param region name of variable used to split up regions
#' @param ... not used by this method
#' @name fortify.sp
#' @examples
#' if (require("maptools")) {
#'  sids <- system.file("shapes/sids.shp", package="maptools")
#'  nc1 <- readShapePoly(sids,
#'    proj4string = CRS("+proj=longlat +datum=NAD27"))
#'  nc1_df <- fortify(nc1)
#' }
NULL

#' @rdname fortify.sp
#' @export
#' @method fortify SpatialPolygonsDataFrame
fortify.SpatialPolygonsDataFrame <- function(model, data, region = NULL, ...) {
  attr <- as.data.frame(model)
  # If not specified, split into regions based on polygons
  if (is.null(region)) {
    coords <- plyr::ldply(model@polygons,fortify)
    message("Regions defined for each Polygons")
  } else {
    cp <- sp::polygons(model)

    # Union together all polygons that make up a region
    unioned <- maptools::unionSpatialPolygons(cp, attr[, region])
    coords <- fortify(unioned)
    coords$order <- 1:nrow(coords)
  }
  coords
}

#' @rdname fortify.sp
#' @export
#' @method fortify SpatialPolygons
fortify.SpatialPolygons <- function(model, data, ...) {
  plyr::ldply(model@polygons, fortify)
}

#' @rdname fortify.sp
#' @export
#' @method fortify Polygons
fortify.Polygons <- function(model, data, ...) {
  subpolys <- model@Polygons
  pieces <- plyr::ldply(seq_along(subpolys), function(i) {
    df <- fortify(subpolys[[model@plotOrder[i]]])
    df$piece <- i
    df
  })

  pieces$order <- 1:nrow(pieces)
  pieces$id <- model@ID
  pieces$piece <- factor(pieces$piece)
  pieces$group <- interaction(pieces$id, pieces$piece)
  pieces
}

#' @rdname fortify.sp
#' @export
#' @method fortify Polygon
fortify.Polygon <- function(model, data, ...) {
  df <- as.data.frame(model@coords)
  names(df) <- c("long", "lat")
  df$order <- 1:nrow(df)
  df$hole <- model@hole
  df
}

#' @rdname fortify.sp
#' @export
#' @method fortify SpatialLinesDataFrame
fortify.SpatialLinesDataFrame <- function(model, data, ...) {
  plyr::ldply(model@lines, fortify)
}

#' @rdname fortify.sp
#' @export
#' @method fortify Lines
fortify.Lines <- function(model, data, ...) {
  lines <- model@Lines
  pieces <- plyr::ldply(seq_along(lines), function(i) {
    df <- fortify(lines[[i]])
    df$piece <- i
    df
  })

  pieces$order <- 1:nrow(pieces)
  pieces$id <- model@ID
  pieces$piece <- factor(pieces$piece)
  pieces$group <- interaction(pieces$id, pieces$piece)
  pieces
}

#' @rdname fortify.sp
#' @export
#' @method fortify Line
fortify.Line <- function(model, data, ...) {
  df <- as.data.frame(model@coords)
  names(df) <- c("long", "lat")
  df$order <- 1:nrow(df)
  df
}
