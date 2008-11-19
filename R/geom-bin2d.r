GeomBin2d <- proto(Geom, {
  draw <- function(., data, scales, coordinates, ...) {
    GeomRect$draw(data, scales, coordinates, ...)
  }

  objname <- "bin2d"
  desc <- "Add heatmap of 2d bin counts"
  icon <- function(.) {
  }
  
  guide_geom <- function(.) "polygon"
  
  default_stat <- function(.) StatBin2d
  required_aes <- c("xmin", "xmax", "ymin", "ymax")
  default_aes <- function(.) {
    aes(colour = NA, fill = "grey60", size = 0.5, linetype = 1, weight = 1)
  }

  examples <- function(.) {
    # See ?stat_bin2d
  }

})
