#' Visualise sf objects
#'
#' This set of geom, stat, and coord are used to visualise simple feature (sf)
#' objects. For simple plots, you will only need `geom_sf()` as it
#' uses `stat_sf()` and adds `coord_sf()` for you. `geom_sf()` is
#' an unusual geom because it will draw different geometric objects depending
#' on what simple features are present in the data: you can get points, lines,
#' or polygons.
#' For text and labels, you can use `geom_sf_text()` and `geom_sf_label()`.
#'
#' @section Geometry aesthetic:
#' `geom_sf()` uses a unique aesthetic: `geometry`, giving an
#' column of class `sfc` containing simple features data. There
#' are three ways to supply the `geometry` aesthetic:
#'
#'   - Do nothing: by default `geom_sf()` assumes it is stored in
#'     the `geometry` column.
#'   - Explicitly pass an `sf` object to the `data` argument.
#'     This will use the primary geometry column, no matter what it's called.
#'   - Supply your own using `aes(geometry = my_column)`
#'
#' Unlike other aesthetics, `geometry` will never be inherited from
#' the plot.
#'
#' @section CRS:
#' `coord_sf()` ensures that all layers use a common CRS. You can
#' either specify it using the `CRS` param, or `coord_sf()` will
#' take it from the first layer that defines a CRS.
#'
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'
#'   You can also set this to one of "polygon", "line", and "point" to
#'   override the default legend.
#' @seealso [stat_sf_coordinates()]
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' ggplot(nc) +
#'   geom_sf(aes(fill = AREA))
#'
#' # If not supplied, coord_sf() will take the CRS from the first layer
#' # and automatically transform all other layers to use that CRS. This
#' # ensures that all data will correctly line up
#' nc_3857 <- sf::st_transform(nc, "+init=epsg:3857")
#' ggplot() +
#'   geom_sf(data = nc) +
#'   geom_sf(data = nc_3857, colour = "red", fill = NA)
#'
#' # Unfortunately if you plot other types of feature you'll need to use
#' # show.legend to tell ggplot2 what type of legend to use
#' nc_3857$mid <- sf::st_centroid(nc_3857$geometry)
#' ggplot(nc_3857) +
#'   geom_sf(colour = "white") +
#'   geom_sf(aes(geometry = mid, size = AREA), show.legend = "point")
#'
#' # You can also use layers with x and y aesthetics: these are
#' # assumed to already be in the common CRS.
#' ggplot(nc) +
#'   geom_sf() +
#'   annotate("point", x = -80, y = 35, colour = "red", size = 4)
#'
#' # Thanks to the power of sf, a geom_sf nicely handles varying projections
#' # setting the aspect ratio correctly.
#' library(maps)
#' world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
#' ggplot() + geom_sf(data = world1)
#'
#' world2 <- sf::st_transform(
#'   world1,
#'   "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs"
#' )
#' ggplot() + geom_sf(data = world2)
#'
#' # To add labels, use geom_sf_label().
#' ggplot(nc_3857[1:3, ]) +
#'    geom_sf(aes(fill = AREA)) +
#'    geom_sf_label(aes(label = NAME))
#' }
#' @name ggsf
NULL

