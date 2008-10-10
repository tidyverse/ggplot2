GeomHexbin <- proto(Geom, {
  objname <- "hexbin"
  draw <- function(., data, scales, coordinates, ...) { 
    with(coordinates$transform(data), 
      ggname(.$my_name(), hexGrob(x, y, col=colour, fill = fill))
    )
  }
  
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour=NA, fill = "grey50", size=0.5)
  default_stat <- function(.) StatHexbin
  guide_geom <- function(.) "tile"
  
})


# Modified from code by Nicholas Lewin-Koh and Martin Maechler
hexGrob <- function(x, y, size = rep(1, length(x)), colour = "grey50", fill = "grey90") {
  stopifnot(length(y) == length(x))
  
  dx <- resolution(x, FALSE)
  dy <- resolution(y, FALSE) / sqrt(3) / 2 * 1.15
  
  hexC <- hexcoords(dx, dy, n = 1)
  
  n <- length(x)

  polygonGrob(
    x = rep.int(hexC$x, n) * rep(size, each = 6) + rep(x, each = 6),
    y = rep.int(hexC$y, n) * rep(size, each = 6) + rep(y, each = 6),
    default.units = "native",
    id.lengths = rep(6, n), gp = gpar(col = colour, fill = fill)
  )
}