#' Extract coordinates from 'sf' objects
#'
#' `stat_sf_coordinates()` extracts the coordinates from 'sf' objects and
#' summarises them to one pair of coordinates (X and Y, and possibly Z and/or M)
#' per geometry. This is convenient when you draw an sf object as geoms like
#' texts and labels (so [geom_sf_text()] and [geom_sf_label()] relies on this).
#'
#' @rdname stat_sf_coordinates
#' @details
#' coordinates of an `sf` object can be retrieved by [sf::st_coordinates()].
#' But, we cannot simply use `sf::st_coordinates()` because, whereas texts and
#' labels require exactly one coordinate per geometry, it returns multiple ones
#' for a polygon or a line. Thus, these two steps are needed:
#'
#' 1. Choose one point per geometry by some function like `sf::st_centroid()`
#'    and `sf::st_point_on_surface()`.
#' 2. Retrieve coordinates from the points by `sf::st_coordinates()`.
#'
#' For the first step, you can use an arbitrary function via `fun.geometry`.
#' By default, [sf::st_point_on_surface()] is used; This seems more appropriate
#' than [sf::st_centroid()] since lables and texts usually are intended to be
#' put within the polygon or the line.
#' 
#' @section Computed variables:
#' \describe{
#'   \item{X}{X dimension of the simple feature}
#'   \item{Y}{Y dimension of the simple feature}
#'   \item{Z}{Z dimension of the simple feature (if available)}
#'   \item{M}{M dimension of the simple feature (if available)}
#' }
#'
#' Note that, while Z and M dimensions are theoretically available, you may
#' face errors because sf functions don't always support Z and M. In such cases,
#' you can drop these dimensions either beforehand or in a custom `fun.geometry`
#' by [sf::st_zm()].
#' 
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#' storms <- sf::st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)
#' 
#' ggplot(storms) +
#'   stat_sf_coordinates()
#' 
#' ggplot(storms) +
#'   stat_sf_coordinates(aes(colour = stat(Z)))
#' }
#' 
#' @export
#' @inheritParams stat_identity
#' @inheritParams geom_point
#' @param fun.geometry
#'   A function that takes a `sfc` object and returns a `sfc_POINT` with the
#'   same length as the input (e.g. [sf::st_point_on_surface()]). Note that the
#'   function may warn about the incorrectness of the result if the data is not
#'   projected, but you can ignore this except when you are very careful about
#'   the exact locations.
stat_sf_coordinates <- function(mapping = aes(), data = NULL, geom = "point",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE,
                                fun.geometry = sf::st_point_on_surface,
                                ...) {
  # Automatically determin name of geometry column
  if (!is.null(data) && is_sf(data)) {
    geometry_col <- attr(data, "sf_column")
  } else {
    geometry_col <- "geometry"
  }
  if (is.null(mapping$geometry)) {
    mapping$geometry <- as.name(geometry_col)
  }

  layer(
    stat = StatSfCoordinates,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
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
  compute_group = function(data, scales, fun.geometry = sf::st_point_on_surface) {
    points_sfc <- fun.geometry(data$geometry)
    coordinates <- sf::st_coordinates(points_sfc)
    data <- cbind(data, coordinates)

    data
  },

  default_aes = aes(x = stat(X), y = stat(Y)),
  required_aes = c("geometry")
)
