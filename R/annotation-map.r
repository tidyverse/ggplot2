#' @include geom-map.r
NULL

#' Annotation: maps.
#'
#' @param map data frame representing a map.  Most map objects can be 
#'   converted into the right format by using \code{\link{fortify}}
#' @param ... other arguments used to modify aesthetics
#' @export
#' @examples
#' library(maps)
#' usamap <- map_data("state")
#' 
#' seal.sub <- subset(seals, long > -130 & lat < 45 & lat > 40)
#' ggplot(seal.sub, aes(x = long, y = lat)) +
#'   annotation_map(usamap, fill = "NA", colour = "grey50") +
#'   geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat))
#' 
#' seal2 <- transform(seal.sub,
#'   latr = cut(lat, 2),
#'   longr = cut(long, 2))
#' 
#' ggplot(seal2,  aes(x = long, y = lat)) +
#'   annotation_map(usamap, fill = "NA", colour = "grey50") +
#'   geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat)) +
#'   facet_grid(latr ~ longr, scales = "free", space = "free")
annotation_map <- function(map, ...) { 

  # Get map input into correct form
  stopifnot(is.data.frame(map))
  if (!is.null(map$lat)) map$y <- map$lat
  if (!is.null(map$long)) map$x <- map$long
  if (!is.null(map$region)) map$id <- map$region
  stopifnot(all(c("x", "y", "id") %in% names(map)))
  
  GeomAnnotationMap$new(geom_params = list(map = map, ...), data =
    NULL, inherit.aes = FALSE)
}

GeomAnnotationMap <- gg(proto(GeomMap, {
  objname <- "map"

  draw_groups <- function(., data, scales, coordinates, map, ...) {
    # Munch, then set up id variable for polygonGrob -
    # must be sequential integers
    coords <- coord_munch(coordinates, map, scales)
    coords$group <- coords$group %||% coords$id
    grob_id <- match(coords$group, unique(coords$group))

    polygonGrob(coords$x, coords$y, default.units = "native",
      id = grob_id,
      gp = gpar(
        col = data$colour, fill = alpha(data$fill, data$alpha), 
        lwd = data$size * .pt))
  }
  
  required_aes <- c()
  
}))
