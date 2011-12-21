#' @include geom-polygon.r
NULL

#' Polygons from a reference map.
#'
#' Does not affect position scales.  
#' 
#' @export
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
#' crimesm <- melt(crimes, id = 1)
#' if (require(maps)) {
#'   states_map <- map_data("state")
#'   ggplot(crimes, aes(map_id = state)) + geom_map(aes(fill = Murder), map = states_map) + expand_limits(x = states_map$long, y = states_map$lat)
#'   ggplot(crimesm, aes(map_id = state)) + geom_map(aes(fill = value), map = states_map) + expand_limits(x = states_map$long, y = states_map$lat) + facet_wrap( ~ variable)
#' }
#' 
geom_map <- function(mapping = NULL, data = NULL, map, stat = "identity", ...) { 

  # Get map input into correct form
  stopifnot(is.data.frame(map))
  if (!is.null(map$lat)) map$y <- map$lat
  if (!is.null(map$long)) map$x <- map$long
  if (!is.null(map$region)) map$id <- map$region
  stopifnot(all(c("x", "y", "id") %in% names(map)))
  map <- split(map, map$id)
  
  GeomMap$new(geom_params = list(map = map), mapping = mapping, data = data, stat = stat, ...)
}

GeomMap <- proto(GeomPolygon, {
  objname <- "map"

  draw_groups <- function(., data, scales, coordinates, map, ...) {
    if (!is.null(data$map_id))
    data <- data[data$map_id %in% names(map), , drop = FALSE]

    polys <- rbind.fill(map[data$map_id])
    id <- match(polys$group, unique(polys$group))
    coords <- coord_munch(coordinates, polys, scales)

    polygonGrob(coords$x, coords$y, default.units = "native", id = id,
      gp = gpar(
        col = data$colour, fill = alpha(data$fill, data$alpha), 
        lwd = data$size * .pt))
  }
  
  required_aes <- c("map_id")
  
})
