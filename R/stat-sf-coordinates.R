#' Extract coordinates from 'sf' objects
#'
#' `stat_sf_coordinates()` extracts the coordinates from 'sf' objects and
#' summarises them to one pair of coordinates (x and y) per geometry. This is
#' convenient when you draw an sf object as geoms like text and labels (so
#' [geom_sf_text()] and [geom_sf_label()] relies on this).
#'
#' @rdname stat_sf_coordinates
#' @details
#' coordinates of an `sf` object can be retrieved by `sf::st_coordinates()`.
#' But, we cannot simply use `sf::st_coordinates()` because, whereas text and
#' labels require exactly one coordinate per geometry, it returns multiple ones
#' for a polygon or a line. Thus, these two steps are needed:
#'
#' 1. Choose one point per geometry by some function like `sf::st_centroid()`
#'    or `sf::st_point_on_surface()`.
#' 2. Retrieve coordinates from the points by `sf::st_coordinates()`.
#'
#' For the first step, you can use an arbitrary function via `fun.geometry`.
#' By default, `function(x) sf::st_point_on_surface(sf::st_zm(x))` is used;
#' `sf::st_point_on_surface()` seems more appropriate than `sf::st_centroid()`
#' since labels and text usually are intended to be put within the polygon or
#' the line. `sf::st_zm()` is needed to drop Z and M dimension beforehand,
#' otherwise `sf::st_point_on_surface()` may fail when the geometries have M
#' dimension.
#'
#' @eval rd_computed_vars(
#'  x = "X dimension of the simple feature.",
#'  y = "Y dimension of the simple feature."
#' )
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#'
#' ggplot(nc) +
#'   stat_sf_coordinates()
#'
#' ggplot(nc) +
#'   geom_errorbarh(
#'     aes(geometry = geometry,
#'         xmin = after_stat(x) - 0.1,
#'         xmax = after_stat(x) + 0.1,
#'         y = after_stat(y),
#'         height = 0.04),
#'     stat = "sf_coordinates"
#'   )
#' }
#'
#' @export
#' @inheritParams stat_identity
#' @inheritParams geom_point
#' @param fun.geometry
#'   A function that takes a `sfc` object and returns a `sfc_POINT` with the
#'   same length as the input. If `NULL`, `function(x) sf::st_point_on_surface(sf::st_zm(x))`
#'   will be used. Note that the function may warn about the incorrectness of
#'   the result if the data is not projected, but you can ignore this except
#'   when you really care about the exact locations.
stat_sf_coordinates <- function(mapping = aes(), data = NULL, geom = "point",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE,
                                fun.geometry = NULL,
                                ...) {
  layer_sf(
    stat = StatSfCoordinates,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      ...
    )
  )
}

#' @rdname stat_sf_coordinates
#' @usage NULL
#' @format NULL
#' @export
StatSfCoordinates <- ggproto(
  "StatSfCoordinates", Stat,

  compute_layer = function(self, data, params, layout) {
    # add coord to the params, so it can be forwarded to compute_group()
    params$coord <- layout$coord
    ggproto_parent(Stat, self)$compute_layer(data, params, layout)
  },

  compute_group = function(self, data, scales, coord, fun.geometry = NULL) {
    if (is.null(fun.geometry)) {
      fun.geometry <- function(x) sf::st_point_on_surface(sf::st_zm(x))
    }

    points_sfc <- fun.geometry(data$geometry)

    if (inherits(coord, "CoordSf")) {
      # register bounding box if the coord derives from CoordSf
      bbox <- sf::st_bbox(points_sfc)

      coord$record_bbox(
        xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
        ymin = bbox[["ymin"]], ymax = bbox[["ymax"]]
      )

      # transform to the coord's default crs if possible
      default_crs <- coord$get_default_crs()
      if (!(is.null(default_crs) || is.na(default_crs) ||
            is.na(sf::st_crs(points_sfc)))) {
        points_sfc <- sf::st_transform(points_sfc, default_crs)
      }
    }
    coordinates <- sf::st_coordinates(points_sfc)
    data$x <- coordinates[, "X"]
    data$y <- coordinates[, "Y"]

    data
  },

  default_aes = aes(x = after_stat(x), y = after_stat(y)),
  required_aes = c("geometry")
)
