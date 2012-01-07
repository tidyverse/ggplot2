#' @include geom-.r
NULL

#' High-performance rectangular tiling.
#'
#' This is a special case of \code{\link{geom_tile}} where all tiles are
#' the same size.  It is implemented highly efficiently using the internal
#' \code{rasterGrob} function.
#'
#' @inheritParams geom_point
#' @export
#' @examples
#' # Generate data
#' pp <- function (n,r=4) {
#'  x <- seq(-r*pi, r*pi, len=n)
#'  df <- expand.grid(x=x, y=x)
#'  df$r <- sqrt(df$x^2 + df$y^2)
#'  df$z <- cos(df$r^2)*exp(-df$r/6)
#'  df
#' }
#' qplot(x, y, data = pp(20), fill = z, geom = "raster")
#'
#' # For the special cases where it is applicable, geom_raster is much
#' # faster than geom_tile:
#' pp200 <- pp(200)
#' base <- ggplot(pp200, aes(x, y, fill = z))
#' benchplot(base + geom_raster())
#' benchplot(base + geom_tile())
geom_raster <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) { 
  GeomRaster$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomRaster <- proto(Geom, {
  objname <- "raster"
  draw <- function(., data, scales, coordinates, ...) {
    if (!inherits(coordinates, "cartesian")) {
      stop("geom_raster only works with Cartesian coordinates", call. = FALSE)
    }
    data <- coord_transform(coordinates, data, scales)
    raster <- acast(data, list("y", "x"), value.var = "fill")
    
    width <- resolution(data$x, zero = FALSE)
    height <- resolution(data$y, zero = FALSE)
    
    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)

    rasterGrob(raster[nrow(raster):1, , drop = FALSE], x_rng[1] - width / 2, y_rng[1] - height / 2, 
      diff(x_rng) + width, diff(y_rng) + height, default.units = "native", 
      just = c("left","bottom"), interpolate = FALSE)
  }


  icon <- function(.) {
    rectGrob(c(0.25, 0.25, 0.75, 0.75), c(0.25, 0.75, 0.75, 0.25), width=0.5, height=c(0.67, 0.5, 0.67, 0.5), gp=gpar(col="grey20", fill=c("#804070", "#668040")))
  }

  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(fill = "grey20", alpha = 1)
  required_aes <- c("x", "y")
  guide_geom <- function(.) "polygon"
})
