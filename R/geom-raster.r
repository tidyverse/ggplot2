#' @include geom-.r
NULL

#' High-performance rectangular tiling.
#'
#' This is a special case of \code{\link{geom_tile}} where all tiles are
#' the same size.  It is implemented highly efficiently using the internal
#' \code{rasterGrob} function.
#'
#' By default, \code{geom_raster} add a vertical and horizontal padding.
#' The size of padding depends on the resolution of data.
#' If you want to manually set the padding (e.g. want zero-padding),
#' you can change the behavior by setting \code{hpad} and \code{vpad}.
#'
#' @inheritParams geom_point
#' @param hpad,vpad horizontal and vertical padding in data unit.
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
#'
#' # padding
#' df <- expand.grid(x = 0:5, y = 0:5)
#' df$z <- runif(nrow(df))
#' # default is compatible with geom_tile()
#' ggplot(df, aes(x, y, fill = z)) + geom_raster()
#' # zero padding
#' ggplot(df, aes(x, y, fill = z)) + geom_raster(hpad = 0, vpad = 0)

geom_raster <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", hpad = NULL, vpad = NULL, ...) { 
  GeomRaster$new(mapping = mapping, data = data, stat = stat, position = position, hpad = hpad, vpad = vpad, ...)
}

GeomRaster <- proto(Geom, {
  objname <- "raster"
  
  reparameterise <- function(., df, params) {
    df$hpad <- df$hpad %||% params$hpad %||% resolution(df$x, FALSE)
    df$vpad <- df$vpad %||% params$vpad %||% resolution(df$y, FALSE)
    
    transform(df, 
      xmin = x - hpad / 2,  xmax = x + hpad / 2, hpad = NULL,
      ymin = y - vpad / 2, ymax = y + vpad / 2, vpad = NULL
    )
  }
  
  draw <- function(., data, scales, coordinates, hpad = NULL, vpad = NULL, ...) {

    if (!inherits(coordinates, "cartesian")) {
      stop("geom_raster only works with Cartesian coordinates", call. = FALSE)
    }
    data <- coord_transform(coordinates, data, scales)
    raster <- acast(data, list("y", "x"), value.var = "fill")
    raster <- raster[nrow(raster):1, , drop = FALSE]

    # data range
    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)

    x <- mean(x_rng)
    y <- mean(y_rng)
    w <- abs(max(data$xmax, na.rm = TRUE) - min(data$xmin, na.rm = TRUE))
    h <- abs(max(data$ymax, na.rm = TRUE) - min(data$ymin, na.rm = TRUE))

    rasterGrob(raster, x = x, y = y, width = w, height = h , default.units = "native", interpolate = FALSE)
  }


  icon <- function(.) {
    rectGrob(c(0.25, 0.25, 0.75, 0.75), c(0.25, 0.75, 0.75, 0.25), width=0.5, height=c(0.67, 0.5, 0.67, 0.5), gp=gpar(col="grey20", fill=c("#804070", "#668040")))
  }

  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(fill = "grey20", alpha = 1)
  required_aes <- c("x", "y")
  guide_geom <- function(.) "polygon"
})
