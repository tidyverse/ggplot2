#' @include geom-.r
#' @include geom-raster.r
NULL

#' Annotation: High-performance rectangular tiling.
#'
#' This is a special version of \code{\link{geom_raster}} optimised for static
#' annotations that are the same in every panel. These annotations will not
#' affect scales (i.e. the x and y axes will not grow to cover the range
#' of the raster, and the raster must already have its own colours).
#'
#' Most useful for adding bitmap images.
#'
#' @param raster raster object to display
#' @param xmin,xmax x location (in data coordinates) giving horizontal
#'   location of raster
#' @param ymin,ymax y location (in data coordinates) giving vertical
#'   location of raster
#' @param interpolate If \code{TRUE} interpolate linearly, if \code{FALSE}
#'   (the default) don't interpolate.
#' @export
#' @examples
#' # Generate data
#' rainbow <- matrix(hcl(seq(0, 360, length = 50 * 50), 80, 70), nrow = 50)
#' qplot(mpg, wt, data = mtcars) +
#'   annotation_raster(rainbow, 15, 20, 3, 4)
#' # To fill up whole plot
#' qplot(mpg, wt, data = mtcars) +
#'   annotation_raster(rainbow, -Inf, Inf, -Inf, Inf) +
#'   geom_point()
#'
#' rainbow2 <- matrix(hcl(seq(0, 360, length = 10), 80, 70), nrow = 1)
#' qplot(mpg, wt, data = mtcars) +
#'   annotation_raster(rainbow2, -Inf, Inf, -Inf, Inf) +
#'   geom_point()
#' rainbow2 <- matrix(hcl(seq(0, 360, length = 10), 80, 70), nrow = 1)
#' qplot(mpg, wt, data = mtcars) +
#'   annotation_raster(rainbow2, -Inf, Inf, -Inf, Inf, interpolate = TRUE) +
#'   geom_point()
annotation_raster <- function (raster, xmin, xmax, ymin, ymax, interpolate = FALSE) {
  raster <- as.raster(raster)
  GeomRasterAnn$new(geom_params = list(raster = raster, xmin = xmin,
    xmax = xmax, ymin = ymin, ymax = ymax, interpolate = interpolate),
    stat = "identity", position = "identity", data = NULL, inherit.aes = TRUE)
}

GeomRasterAnn <- proto(GeomRaster, {
  objname <- "raster_ann"
  reparameterise <- function(., df, params) {
    df
  }

  draw_groups <- function(., data, scales, coordinates, raster, xmin, xmax,
    ymin, ymax, interpolate = FALSE, ...) {
    if (!inherits(coordinates, "cartesian")) {
      stop("annotation_raster only works with Cartesian coordinates",
        call. = FALSE)
    }
    corners <- data.frame(x = c(xmin, xmax), y = c(ymin, ymax))
    data <- coord_transform(coordinates, corners, scales)

    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)

    rasterGrob(raster, x_rng[1], y_rng[1],
      diff(x_rng), diff(y_rng), default.units = "native",
      just = c("left","bottom"), interpolate = interpolate)
  }
})
