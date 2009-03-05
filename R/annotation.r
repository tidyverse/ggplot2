# annotate("text", x = 0, y = 0, label = "title")
annotate <- function(geom, x, y, ...) {
  layer(
    geom = geom, geom_params = list(...), 
    stat = "identity", 
    inherit.aes = FALSE,
    data = data.frame(x, y), mapping = aes(x, y)
  )
}
