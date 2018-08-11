#' Extract coordinates from 'sf' objects
#' 
#' @rdname stat_sf_coordinates
#' @section Computed variables:
#' \describe{
#'   \item{X}{X dimension of the simple feature}
#'   \item{Y}{Y dimension of the simple feature}
#'   \item{Z}{Z dimension of the simple feature}
#'   \item{M}{M dimension of the simple feature}
#' }
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
#'   A function that takes a `sfc` object and returns a
#'   `sfc_POINT` with the same length as the input (e.g. [sf::st_point_on_surface()]).
stat_sf_coordinates <- function(mapping = aes(), data = NULL, geom = "point",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, fun.geometry = sf::st_point_on_surface,
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
