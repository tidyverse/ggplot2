#' Draw rectangles.
#'
#' \code{geom_rect} and \code{geom_tile} do the same thing, but are
#' parameterised differently. \code{geom_rect} uses the locations of the four
#' corners (\code{xmin}, \code{xmax}, \code{ymin} and \code{ymax}).
#' \code{geom_tile} uses the center of the tile and its size (\code{x},
#' \code{y}, \code{width}, \code{height}). \code{geom_raster} is a high
#' performance special case for when all the tiles are the same size.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "tile")}
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @export
#' @examples
#' # The most common use for rectangles is to draw a surface. You always want
#' # to use geom_raster here because it's so much faster, and produces
#' # smaller output when saving to PDF
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'  geom_raster(aes(fill = density))
#'
#' # Interpolation smooths the surface & is most helpful when rendering images.
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'  geom_raster(aes(fill = density), interpolate = TRUE)
#'
#' # If you want to draw arbitrary rectangles, use geom_tile() or geom_rect()
#' df <- data.frame(
#'   x = rep(c(2, 5, 7, 9, 12), 2),
#'   y = rep(c(1, 2), each = 5),
#'   z = factor(rep(1:5, each = 2)),
#'   w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
#' )
#' ggplot(df, aes(x, y)) +
#'   geom_tile(aes(fill = z))
#' ggplot(df, aes(x, y)) +
#'   geom_tile(aes(fill = z, width = w), colour = "grey50")
#' ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
#'   geom_rect(aes(fill = z, width = w), colour = "grey50")
#'
#' \donttest{
#' # Justification controls where the cells are anchored
#' df <- expand.grid(x = 0:5, y = 0:5)
#' df$z <- runif(nrow(df))
#' # default is compatible with geom_tile()
#' ggplot(df, aes(x, y, fill = z)) + geom_raster()
#' # zero padding
#' ggplot(df, aes(x, y, fill = z)) + geom_raster(hjust = 0, vjust = 0)
#'
#' # Inspired by the image-density plots of Ken Knoblauch
#' cars <- ggplot(mtcars, aes(mpg, factor(cyl)))
#' cars + geom_point()
#' cars + stat_bin2d(aes(fill = ..count..), binwidth = c(3,1))
#' cars + stat_bin2d(aes(fill = ..density..), binwidth = c(3,1))
#'
#' cars + stat_density(aes(fill = ..density..), geom = "raster", position = "identity")
#' cars + stat_density(aes(fill = ..count..), geom = "raster", position = "identity")
#' }
geom_tile <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.r
GeomTile <- ggproto("GeomTile", GeomRect,
  extra_params = c("na.rm", "width", "height"),

  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

    transform(data,
      xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  default_aes = aes(fill = "grey20", colour = NA, size = 0.1, linetype = 1,
    alpha = NA),

  required_aes = c("x", "y"),

  draw_key = draw_key_polygon
)
