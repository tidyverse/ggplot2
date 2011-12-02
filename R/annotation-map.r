#' @include geom-map.r
NULL

#' 
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

GeomAnnotationMap <- proto(GeomMap, {
  objname <- "map"

  draw_groups <- function(., data, scales, coordinates, map, ...) {
    coords <- coord_munch(coordinates, map, scales)
    id <- match(map$group, unique(map$group))
    
    polygonGrob(coords$x, coords$y, default.units = "native", id = id,
      gp = gpar(
        col = data$colour, fill = alpha(data$fill, data$alpha), 
        lwd = data$size * .pt))
  }
  
  required_aes <- c()
  
})
