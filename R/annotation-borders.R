#' Create a layer of map borders
#'
#' This is a quick and dirty way to get map data (from the \pkg{maps} package)
#' onto your plot. This is a good place to start if you need some crude
#' reference lines, but you'll typically want something more sophisticated
#' for communication graphics.
#'
#' @param database map data, see [maps::map()] for details
#' @param regions map region
#' @param fill fill colour
#' @param colour border colour
#' @param xlim,ylim latitudinal and longitudinal ranges for extracting map
#'   polygons, see [maps::map()] for details.
#' @inheritDotParams geom_polygon
#' @export
#' @examples
#' if (require("maps")) {
#'
#' ia <- map_data("county", "iowa")
#' mid_range <- function(x) mean(range(x))
#' seats <- do.call(rbind, lapply(split(ia, ia$subregion), function(d) {
#'   data.frame(lat = mid_range(d$lat), long = mid_range(d$long), subregion = unique(d$subregion))
#' }))
#'
#' ggplot(ia, aes(long, lat)) +
#'   geom_polygon(aes(group = group), fill = NA, colour = "grey60") +
#'   geom_text(aes(label = subregion), data = seats, size = 2, angle = 45)
#' }
#'
#' if (require("maps")) {
#' data(us.cities)
#' capitals <- subset(us.cities, capital == 2)
#' ggplot(capitals, aes(long, lat)) +
#'   annotation_borders("state") +
#'   geom_point(aes(size = pop)) +
#'   scale_size_area() +
#'   coord_quickmap()
#' }
#'
#' if (require("maps")) {
#' # Same map, with some world context
#' ggplot(capitals, aes(long, lat)) +
#'   annotation_borders("world", xlim = c(-130, -60), ylim = c(20, 50)) +
#'   geom_point(aes(size = pop)) +
#'   scale_size_area() +
#'   coord_quickmap()
#' }
annotation_borders <- function(database = "world", regions = ".", fill = NA,
                               colour = "grey50", xlim = NULL, ylim = NULL, ...) {
  df <- map_data(database, regions, xlim = xlim, ylim = ylim)
  geom_polygon(aes(.data$long, .data$lat, group = .data$group), data = df,
               fill = fill, colour = colour, ..., inherit.aes = FALSE)
}
