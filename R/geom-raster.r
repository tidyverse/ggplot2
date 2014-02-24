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
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "raster")}
#'
#' @inheritParams geom_point
#' @param hjust,vjust horizontal and vertical justification of the grob.  Each
#'   justification value should be a number between 0 and 1.  Defaults to 0.5
#'   for both, centering each pixel over its data location.
#' @param interpolate If \code{TRUE} interpolate linearly, if \code{FALSE}
#'   (the default) don't interpolate.
#' @export
#' @examples
#' \donttest{
#' # Generate data
#' pp <- function (n,r=4) {
#'  x <- seq(-r*pi, r*pi, len=n)
#'  df <- expand.grid(x=x, y=x)
#'  df$r <- sqrt(df$x^2 + df$y^2)
#'  df$z <- cos(df$r^2)*exp(-df$r/6)
#'  df
#' }
#' qplot(x, y, data = pp(20), fill = z, geom = "raster")
#' # Interpolation worsens the apperance of this plot, but can help when
#' # rendering images.
#' qplot(x, y, data = pp(20), fill = z, geom = "raster", interpolate = TRUE)
#'
#' # For the special cases where it is applicable, geom_raster is much
#' # faster than geom_tile:
#' pp200 <- pp(200)
#' base <- ggplot(pp200, aes(x, y, fill = z))
#' benchplot(base + geom_raster())
#' benchplot(base + geom_tile())
#'
#' # justification
#' df <- expand.grid(x = 0:5, y = 0:5)
#' df$z <- runif(nrow(df))
#' # default is compatible with geom_tile()
#' ggplot(df, aes(x, y, fill = z)) + geom_raster()
#' # zero padding
#' ggplot(df, aes(x, y, fill = z)) + geom_raster(hjust = 0, vjust = 0)
#' }
geom_raster <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", hjust = 0.5, vjust = 0.5, interpolate = FALSE, ...) {
  stopifnot(is.numeric(hjust), length(hjust) == 1)
  stopifnot(is.numeric(vjust), length(vjust) == 1)

  GeomRaster$new(mapping = mapping, data = data, stat = stat, position = position, hjust = hjust, vjust = vjust, interpolate = interpolate, ...)
}

GeomRaster <- proto(Geom, {
  objname <- "raster"

  reparameterise <- function(., df, params) {
    hjust <- params$hjust %||% 0.5
    vjust <- params$vjust %||% 0.5

    w <- resolution(df$x, FALSE)
    h <- resolution(df$y, FALSE)

    df$xmin <- df$x - w * (1 - hjust)
    df$xmax <- df$x + w * hjust
    df$ymin <- df$y - h * (1 - vjust)
    df$ymax <- df$y + h * vjust
    df
  }

  # This is a dummy function to make sure that vjust and hjust are recongised
  # as parameters and are accessible to reparameterise.
  draw <- function(vjust = 0.5, hjust = 0.5) {}

  draw_groups <- function(., data, scales, coordinates, interpolate = FALSE, ...) {
    if (!inherits(coordinates, "cartesian")) {
      stop("geom_raster only works with Cartesian coordinates", call. = FALSE)
    }
    data <- remove_missing(data, TRUE, c("x", "y", "fill"),
      name = "geom_raster")
    data <- coord_transform(coordinates, data, scales)

    # Convert vector of data to raster
    x_pos <- as.integer((data$x - min(data$x)) / resolution(data$x, FALSE))
    y_pos <- as.integer((data$y - min(data$y)) / resolution(data$y, FALSE))

    nrow <- max(y_pos) + 1
    ncol <- max(x_pos) + 1

    raster <- matrix(NA_character_, nrow = nrow, ncol = ncol)
    raster[cbind(nrow - y_pos, x_pos + 1)] <- alpha(data$fill, data$alpha)

    # Figure out dimensions of raster on plot
    x_rng <- c(min(data$xmin, na.rm = TRUE), max(data$xmax, na.rm = TRUE))
    y_rng <- c(min(data$ymin, na.rm = TRUE), max(data$ymax, na.rm = TRUE))

    rasterGrob(raster, x = mean(x_rng), y = mean(y_rng),
      width = diff(x_rng), height = diff(y_rng),
      default.units = "native", interpolate = interpolate)
  }

  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(fill = "grey20", alpha = NA)
  required_aes <- c("x", "y")
  guide_geom <- function(.) "polygon"
})
