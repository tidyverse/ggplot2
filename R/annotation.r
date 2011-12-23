#' Annotate a plot
#' 
#' @param geom name of geom to use for annotation
#' @param x x position
#' @param y y position
#' @param xmin xmin position
#' @param ymin ymin position
#' @param xmax xmax position
#' @param ymax ymax position
#' @param ... other arguments passed to geom as parameters
#' @export
#' @examples
#' annotate("text", x = 0, y = 0, label = "title")
annotate <- function(geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, ...) {
  
  layer_data <- compact(list(
    x = x, xmin = xmin, xmax = xmax, 
    y = y, ymin = ymin, ymax = ymax
  ))
  
  layer(
    geom = geom, geom_params = list(...), 
    stat = "identity", 
    inherit.aes = FALSE,
    data = data.frame(layer_data), mapping = aes_all(names(layer_data)),
    show_guide = FALSE
  )
}

