GeomHexbin <- proto(Geom, {
  objname <- "hexbin"
  draw <- function(., data, scales, coordinates, binwidth, ...) { 
    # Need to convert binwidth to [0, 1], in the same way that the 
    # data was
    
    binwidth <- c(
      coordinates$transform_x(binwidth[1]),
      coordinates$transform_y(binwidth[2])
    )
    # binwidth <- binwidth /
    #   c(diff(scales$get_scales("x")$output_set()),
    #     diff(scales$get_scales("y")$output_set()))
     
    with(coordinates$transform(data), 
      ggname(.$my_name(), hexGrob(x, y, binwidth, col=colour, fill = fill))
    )
  }
  
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour=NA, fill = "grey50", size=0.5)
  default_stat <- function(.) StatHexbin
  guide_geom <- function(.) "tile"
  
})


# Modified from code by Nicholas Lewin-Koh and Martin Maechler
hexGrob <- function(x, y, binwidth, size = rep(1, length(x)), colour = "grey50", fill = "grey90") {
  stopifnot(length(binwidth) == 2)
  stopifnot(length(y) == length(x))
  
  dx <- resolution(x)
  dy <- resolution(y) / sqrt(3) / 2
  print(c(dx, dy))
    
  hexC <- hexcoords(dx, dy, n = 1)
  
  n <- length(x)

  polygonGrob(
    x = rep.int(hexC$x, n) * rep(size, each = 6) + rep(x, each = 6),
    y = rep.int(hexC$y, n) * rep(size, each = 6) + rep(y, each = 6),
    default.units = "native",
    id.lengths = rep(6, n), gp = gpar(col = colour, fill = fill)
  )
}