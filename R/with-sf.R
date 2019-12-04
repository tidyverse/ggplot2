#' Turn regular layers into sf layers
#'
#' @param ... Layer
#' @export
with_sf <- function(...) {
  l <- list(...)[[1]] # can handle only one layer for now
  parent_geom <- l$geom
  ggproto(NULL, l,
    geom = ggproto('SfedGeom', parent_geom,
      draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        sfed_coord <- ggproto('SfedCoord', coord,
          transform = function(data, range) {
            data <- sfed_transform(data, crs = coord$crs)
            coord$transform(data, range)
          }
        )
        parent_geom$draw_panel(data, panel_params, sfed_coord, na.rm)
      }
    )
  )
}


sfed_transform <- function(data, crs = NULL) {
  if (is.null(crs)) {
    return(data)
  }

  sf_data <- sf::st_sfc(
    sf::st_multipoint(cbind(data$x, data$y)),
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )
  sf_data_trans <- sf::st_transform(sf_data, crs)[[1]]
  data$x <- sf_data_trans[, 1]
  data$y <- sf_data_trans[, 2]
  data
}
