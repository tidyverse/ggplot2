#' @include geom-map.r
NULL

#' Annotation: a map
#'
#' Display a fixed map on a plot. This function predates the [`geom_sf()`]
#' framework and does not work with sf geometry columns as input. However,
#' it can be used in conjunction with `geom_sf()` layers and/or
#' [`coord_sf()`] (see examples).
#'
#' @param map Data frame representing a map. See [`geom_map()`] for
#'   details.
#' @param ... Other arguments used to modify visual parameters, such as
#'   `colour` or `fill`.
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("maps", quietly = TRUE)) {
#' # location of cities in North Carolina
#' df <- data.frame(
#'   name = c("Charlotte", "Raleigh", "Greensboro"),
#'   lat = c(35.227, 35.772, 36.073),
#'   long = c(-80.843, -78.639, -79.792)
#' )
#'
#' p <- ggplot(df, aes(x = long, y = lat)) +
#'   annotation_map(
#'     map_data("state"),
#'     fill = "antiquewhite", colour = "darkgrey"
#'   ) +
#'   geom_point(color = "blue") +
#'   geom_text(
#'     aes(label = name),
#'     hjust = 1.105, vjust = 1.05, color = "blue"
#'   )
#'
#' # use without coord_sf() is possible but not recommended
#' p + xlim(-84, -76) + ylim(34, 37.2)
#'
#' if (requireNamespace("sf", quietly = TRUE)) {
#' # use with coord_sf() for appropriate projection
#' p +
#'   coord_sf(
#'     crs = st_crs(3347),
#'     default_crs = st_crs(4326),  # data is provided as long-lat
#'     xlim = c(-84, -76),
#'     ylim = c(34, 37.2)
#'   )
#'
#' # you can mix annotation_map() and geom_sf()
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' p +
#'   geom_sf(
#'     data = nc, inherit.aes = FALSE,
#'     fill = NA, color = "black", size = 0.1
#'   ) +
#'   coord_sf(crs = st_crs(3347), default_crs = st_crs(4326))
#' }}}
annotation_map <- function(map, ...) {
  # Get map input into correct form
  if (!is.data.frame(map)) {
    abort("`map` must be a data.frame")
  }
  if (!is.null(map$lat)) map$y <- map$lat
  if (!is.null(map$long)) map$x <- map$long
  if (!is.null(map$region)) map$id <- map$region
  if (!all(c("x", "y", "id") %in% names(map))) {
    abort("`map`must have the columns `x`, `y`, and `id`")
  }

  layer(
    data = dummy_data(),
    stat = StatIdentity,
    geom = GeomAnnotationMap,
    position = PositionIdentity,
    inherit.aes = FALSE,
    params = list(map = map, ...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAnnotationMap <- ggproto("GeomAnnotationMap", GeomMap,
  extra_params = "",
  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_params, coord, map) {
    # Munch, then set up id variable for polygonGrob -
    # must be sequential integers
    coords <- coord_munch(coord, map, panel_params)
    coords$group <- coords$group %||% coords$id
    grob_id <- match(coords$group, unique(coords$group))

    polygonGrob(coords$x, coords$y, default.units = "native",
      id = grob_id,
      gp = gpar(
        col = data$colour, fill = alpha(data$fill, data$alpha),
        lwd = data$size * .pt)
      )
  },

  required_aes = c()
)
