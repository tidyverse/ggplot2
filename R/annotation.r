# annotate("text", x = 0, y = 0, label = "title")
annotate <- function(geom, x, y, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, ...) {
  
  layer_data <- compact(list(
    x = x, xmin = xmin, xmax = xmax, 
    y = y, ymin = ymin, ymax = ymax
  ))
  
  
  layer(
    geom = geom, geom_params = list(...), 
    stat = "identity", 
    inherit.aes = FALSE,
    data = data.frame(layer_data), mapping = aes_all(names(layer_data))
  )
}

aes_all <- function(vars) {
  vars <- rename_aes(vars)
  names(vars) <- vars
  
  structure(
    lapply(vars, function(x) parse(text=x)[[1]]),
    class = "uneval"
  )
  
}