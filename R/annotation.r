# Annotate a plot
# Add annotations to a plot in a convenient manner
# 
# @arguments name of geom to use for annotation
# @arguments x position
# @arguments y position
# @arguments xmin position
# @arguments ymin position
# @arguments xmax position
# @arguments ymax position
# @arguments ... other arguments passed to geom as parameters
# @keyword internal
#X annotate("text", x = 0, y = 0, label = "title")
annotate <- function(geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, ...) {
  
  layer_data <- plyr::compact(list(
    x = x, xmin = xmin, xmax = xmax, 
    y = y, ymin = ymin, ymax = ymax
  ))
  
  layer(
    geom = geom, geom_params = list(...), 
    stat = "identity", 
    inherit.aes = FALSE,
    data = data.frame(layer_data), mapping = aes_all(names(layer_data)),
    legend = FALSE
  )
}

