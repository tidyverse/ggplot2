#' @include geom-polygon.r
NULL

#' Polygons from a reference map.
#'
#' Does not affect position scales.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "map")}
#'
#' @export
#' @param map Data frame that contains the map coordinates.  This will
#'   typically be created using \code{\link{fortify}} on a spatial object.
#'   It must contain columns \code{x} or \code{long}, \code{y} or
#'   \code{lat}, and \code{region} or \code{id}.
#' @inheritParams layer
#' @inheritParams geom_point
#' @examples
#' # When using geom_polygon, you will typically need two data frames:
#' # one contains the coordinates of each polygon (positions),  and the
#' # other the values associated with each polygon (values).  An id
#' # variable links the two together
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
#' ggplot(values) + geom_map(aes(map_id = id), map = positions) +
#'   expand_limits(positions)
#' ggplot(values, aes(fill = value)) +
#'   geom_map(aes(map_id = id), map = positions) +
#'   expand_limits(positions)
#' ggplot(values, aes(fill = value)) +
#'   geom_map(aes(map_id = id), map = positions) +
#'   expand_limits(positions) + ylim(0, 3)
#'
#' # Better example
#' crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#' crimesm <- reshape2::melt(crimes, id = 1)
#' if (require(maps)) {
#'   states_map <- map_data("state")
#'   ggplot(crimes, aes(map_id = state)) +
#'     geom_map(aes(fill = Murder), map = states_map) +
#'     expand_limits(x = states_map$long, y = states_map$lat)
#'
#'   last_plot() + coord_map()
#'   ggplot(crimesm, aes(map_id = state)) +
#'     geom_map(aes(fill = value), map = states_map) +
#'     expand_limits(x = states_map$long, y = states_map$lat) +
#'     facet_wrap( ~ variable)
#' }
geom_map <- function(mapping = NULL, data = NULL,
                     stat = "identity",
                     ...,
                     map,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  # Get map input into correct form
  stopifnot(is.data.frame(map))
  if (!is.null(map$lat)) map$y <- map$lat
  if (!is.null(map$long)) map$x <- map$long
  if (!is.null(map$region)) map$id <- map$region
  stopifnot(all(c("x", "y", "id") %in% names(map)))

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMap,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
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
  draw_panel = function(data, panel_scales, coord, map) {
    # Only use matching data and map ids
    common <- intersect(data$map_id, map$id)
    data <- data[data$map_id %in% common, , drop = FALSE]
    map <- map[map$id %in% common, , drop = FALSE]

    # Munch, then set up id variable for polygonGrob -
    # must be sequential integers
    coords <- coord_munch(coord, map, panel_scales)
    coords$group <- coords$group %||% coords$id
    grob_id <- match(coords$group, unique(coords$group))

    # Align data with map
    data_rows <- match(coords$id[!duplicated(grob_id)], data$map_id)
    data <- data[data_rows, , drop = FALSE]

    polygonGrob(coords$x, coords$y, default.units = "native", id = grob_id,
      gp = gpar(
        col = data$colour, fill = alpha(data$fill, data$alpha),
        lwd = data$size * .pt
      )
    )
  },

  required_aes = c("map_id")
)
