#' Fortify method for map objects
#'
#' This function turns a map into a data frame that can more easily be
#' plotted with ggplot2.
#'
#' @export
#' @seealso [map_data()] and [borders()]
#' @param model map object
#' @param data not used by this method
#' @param ... not used by this method
#' @keywords internal
#' @examples
#' if (require("maps")) {
#' ca <- map("county", "ca", plot = FALSE, fill = TRUE)
#' head(fortify(ca))
#' ggplot(ca, aes(long, lat)) +
#'   geom_polygon(aes(group = group))
#' }
#'
#' if (require("maps")) {
#' tx <- map("county", "texas", plot = FALSE, fill = TRUE)
#' head(fortify(tx))
#' ggplot(tx, aes(long, lat)) +
#'   geom_polygon(aes(group = group), colour = "white")
#' }
fortify.map <- function(model, data, ...) {
  df <- new_data_frame(list(
    long = model$x,
    lat = model$y,
    group = cumsum(is.na(model$x) & is.na(model$y)) + 1,
    order = seq_along(model$x)
  ), n = length(model$x))

  names <- do.call("rbind", lapply(strsplit(model$names, "[:,]"), "[", 1:2))
  df$region <- names[df$group, 1]
  df$subregion <- names[df$group, 2]
  df[stats::complete.cases(df$lat, df$long), ]
}

#' Create a data frame of map data
#'
#' Easily turn data from the \pkg{maps} package in to a data frame suitable
#' for plotting with ggplot2.
#'
#' @param map name of map provided by the \pkg{maps} package.  These
#'   include [maps::county()], [maps::france()],
#'   [maps::italy()], [maps::nz()],
#'   [maps::state()], [maps::usa()],
#'   [maps::world()], [maps::world2()].
#' @param region name of subregions to include.  Defaults to `.` which
#'   includes all subregion.  See documentation for [maps::map()]
#'   for more details.
#' @param exact should the `region` be treated as a regular expression
#'   (`FALSE`) or as a fixed string (`TRUE`).
#' @param ... all other arguments passed on to [maps::map()]
#' @keywords internal
#' @export
#' @examples
#' if (require("maps")) {
#' states <- map_data("state")
#' arrests <- USArrests
#' names(arrests) <- tolower(names(arrests))
#' arrests$region <- tolower(rownames(USArrests))
#'
#' choro <- merge(states, arrests, sort = FALSE, by = "region")
#' choro <- choro[order(choro$order), ]
#' ggplot(choro, aes(long, lat)) +
#'   geom_polygon(aes(group = group, fill = assault)) +
#'   coord_map("albers",  lat0 = 45.5, lat1 = 29.5)
#' }
#'
#' if (require("maps")) {
#' ggplot(choro, aes(long, lat)) +
#'   geom_polygon(aes(group = group, fill = assault / murder)) +
#'   coord_map("albers",  lat0 = 45.5, lat1 = 29.5)
#' }
map_data <- function(map, region = ".", exact = FALSE, ...) {
  check_installed("maps", reason = "for `map_data()`")
  map_obj <- maps::map(map, region, exact = exact, plot = FALSE, fill = TRUE, ...)
  fortify(map_obj)
}

#' Create a layer of map borders
#'
#' This is a quick and dirty way to get map data (from the maps package)
#' on to your plot. This is a good place to start if you need some crude
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
#'   borders("state") +
#'   geom_point(aes(size = pop)) +
#'   scale_size_area() +
#'   coord_quickmap()
#' }
#'
#' if (require("maps")) {
#' # Same map, with some world context
#' ggplot(capitals, aes(long, lat)) +
#'   borders("world", xlim = c(-130, -60), ylim = c(20, 50)) +
#'   geom_point(aes(size = pop)) +
#'   scale_size_area() +
#'   coord_quickmap()
#' }
borders <- function(database = "world", regions = ".", fill = NA,
                    colour = "grey50", xlim = NULL, ylim = NULL, ...) {
  df <- map_data(database, regions, xlim = xlim, ylim = ylim)
  geom_polygon(aes_(~long, ~lat, group = ~group), data = df,
    fill = fill, colour = colour, ..., inherit.aes = FALSE)
}
