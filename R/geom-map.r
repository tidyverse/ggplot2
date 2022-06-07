#' @include geom-polygon.r
NULL

#' Polygons from a reference map
#'
#' Display polygons as a map. This is meant as annotation, so it does not
#' affect position scales. Note that this function predates the [`geom_sf()`]
#' framework and does not work with sf geometry columns as input. However,
#' it can be used in conjunction with `geom_sf()` layers and/or
#' [`coord_sf()`] (see examples).
#'
#' @eval rd_aesthetics("geom", "map")
#' @export
#' @param map Data frame that contains the map coordinates.  This will
#'   typically be created using [fortify()] on a spatial object.
#'   It must contain columns `x` or `long`, `y` or
#'   `lat`, and `region` or `id`.
#' @inheritParams layer
#' @inheritParams geom_point
#' @examples
#' # First, a made-up example containing a few polygons, to explain
#' # how `geom_map()` works. It requires two data frames:
#' # One contains the coordinates of each polygon (`positions`), and is
#' # provided via the `map` argument. The other contains the
#' # other the values associated with each polygon (`values`).  An id
#' # variable links the two together.
#'
#' ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))
#'
#' values <- data.frame(
#'   id = ids,
#'   value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
#' )
#'
#' positions <- data.frame(
#'   id = rep(ids, each = 4),
#'   x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
#'   0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
#'   y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
#'   2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
#' )
#'
#' ggplot(values) +
#'   geom_map(aes(map_id = id), map = positions) +
#'   expand_limits(positions)
#' ggplot(values, aes(fill = value)) +
#'   geom_map(aes(map_id = id), map = positions) +
#'   expand_limits(positions)
#' ggplot(values, aes(fill = value)) +
#'   geom_map(aes(map_id = id), map = positions) +
#'   expand_limits(positions) + ylim(0, 3)
#'
#' # Now some examples with real maps
#' if (require(maps)) {
#'
#'   crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#'
#'   # Equivalent to crimes %>% tidyr::pivot_longer(Murder:Rape)
#'   vars <- lapply(names(crimes)[-1], function(j) {
#'     data.frame(state = crimes$state, variable = j, value = crimes[[j]])
#'   })
#'   crimes_long <- do.call("rbind", vars)
#'
#'   states_map <- map_data("state")
#'
#'   # without geospatial coordinate system, the resulting plot
#'   # looks weird
#'   ggplot(crimes, aes(map_id = state)) +
#'     geom_map(aes(fill = Murder), map = states_map) +
#'     expand_limits(x = states_map$long, y = states_map$lat)
#'
#'   # in combination with `coord_sf()` we get an appropriate result
#'   ggplot(crimes, aes(map_id = state)) +
#'     geom_map(aes(fill = Murder), map = states_map) +
#'     # crs = 5070 is a Conus Albers projection for North America,
#'     #   see: https://epsg.io/5070
#'     # default_crs = 4326 tells coord_sf() that the input map data
#'     #   are in longitude-latitude format
#'     coord_sf(
#'       crs = 5070, default_crs = 4326,
#'       xlim = c(-125, -70), ylim = c(25, 52)
#'     )
#'
#'  ggplot(crimes_long, aes(map_id = state)) +
#'    geom_map(aes(fill = value), map = states_map) +
#'    coord_sf(
#'      crs = 5070, default_crs = 4326,
#'      xlim = c(-125, -70), ylim = c(25, 52)
#'    ) +
#'    facet_wrap(~variable)
#' }
geom_map <- function(mapping = NULL, data = NULL,
                     stat = "identity",
                     ...,
                     map,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  # Get map input into correct form
  if (!is.data.frame(map)) {
    cli::cli_abort("{.arg map} must be a {.cls data.frame}")
  }
  if (!is.null(map$lat)) map$y <- map$lat
  if (!is.null(map$long)) map$x <- map$long
  if (!is.null(map$region)) map$id <- map$region
  if (!all(c("x", "y", "id") %in% names(map))) {
    cli::cli_abort("{.arg map} must have the columns {.col x}, {.col y}, and {.col id}")
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMap,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      map = map,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomMap <- ggproto("GeomMap", GeomPolygon,
  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        linejoin = "round", linemitre = 10, map) {
    # Only use matching data and map ids
    common <- intersect(data$map_id, map$id)
    data <- data[data$map_id %in% common, , drop = FALSE]
    map <- map[map$id %in% common, , drop = FALSE]

    # Munch, then set up id variable for polygonGrob -
    # must be sequential integers
    coords <- coord_munch(coord, map, panel_params)
    coords$group <- coords$group %||% coords$id
    grob_id <- match(coords$group, unique(coords$group))

    # Align data with map
    data_rows <- match(coords$id[!duplicated(grob_id)], data$map_id)
    data <- data[data_rows, , drop = FALSE]

    polygonGrob(coords$x, coords$y, default.units = "native", id = grob_id,
      gp = gpar(
        col = data$colour,
        fill = alpha(data$fill, data$alpha),
        lwd = data$size * .pt,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      )
    )
  },

  required_aes = c("map_id")
)
