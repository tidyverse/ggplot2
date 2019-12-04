#' Turn regular layers into sf layers
#'
#' @param ... Layer
#' @param crs Coordinate reference system of the origin layer. Defaults to
#'   longitude and latitude in WGS84 (EPSG:4326), the World Geodetic System
#'   from 1984.
#' @export
with_sf <- function(..., crs = 4326) {
  x <- list(...)
  with_sf_impl(x, crs)
}

with_sf_impl <- function(x, crs, ...) {
  UseMethod("with_sf_impl")
}

with_sf_impl.default <- function(x, crs, ...) {
  stop(
    "Can't convert object of class `", class(x), "` to an sf layer.\n",
    call. = FALSE
  )
}

with_sf_impl.list <- function(x, crs, ...) {
  l <- list()
  # for some reason lapply() version of this doesn't work
  for (i in seq_along(x)) {
    l[[i]] <- with_sf_impl(x[[i]], crs)
  }
  if (length(l) == 1) return(l[[1]])
  l
}

with_sf_impl.Layer <- function(x, crs, ...) {
  parent_geom <- x$geom
  ggproto(NULL, x,
    geom = ggproto('SfifiedGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        sfified_coord <- ggproto('SfifiedCoord', coord,
          transform = function(data, range) {
            data <- sfified_transform(data, crs = coord$crs, source_crs = crs)
            coord$transform(data, range)
          }
          ## TODO:
          ## need to implement backtransform_range()
        )
        parent_geom$draw_panel(data, panel_params, sfified_coord, na.rm)
      }
    )
  )
}


sfified_transform <- function(data, crs = NULL, source_crs = 4326) {
  if (is.null(crs)) {
    return(data)
  }

  sf_data <- sf::st_sfc(
    sf::st_multipoint(cbind(data$x, data$y)),
    crs = source_crs
  )
  sf_data_trans <- sf::st_transform(sf_data, crs)[[1]]
  data$x <- sf_data_trans[, 1]
  data$y <- sf_data_trans[, 2]
  data
}
