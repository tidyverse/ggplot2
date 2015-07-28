#' Tile plane with rectangles.
#'
#' \code{geom_tile} and \code{geom_raster} draw rectangles. \code{geom_raster}
#' is an efficient special case of where all tiles are the same size.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "tile")}
#'
#' @inheritParams geom_point
#' @export
#' @examples
#' # You almost always want to use geom_raster because it's so much
#' # faster, and produces much smaller output when saving to PDF
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'  geom_raster(aes(fill = density))
#'
#' # Interpolation smooths the surface & is most helpful when rendering images.
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'  geom_raster(aes(fill = density), interpolate = TRUE)
#'
#' # Use geom_tile when you have uneven tile sizes
#' boundary <- c(0, 4, 6, 8, 10, 14)
#' example <- data.frame(
#'   x = rep(c(2, 5, 7, 9, 12), 2),
#'   y = factor(rep(c(1,2), each = 5)),
#'   z = factor(rep(1:5, each = 2)),
#'   w = rep(diff(boundary), 2)
#' )
#' ggplot(example, aes(x, y)) +
#'   geom_tile(aes(fill = z))
#' ggplot(example, aes(x, y)) +
#'   geom_tile(aes(fill = z, width = w), colour = "grey50")
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
geom_tile <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTile <- ggproto("GeomTile", Geom,
  reparameterise = function(df, params) {
    df$width <- df$width %||% params$width %||% resolution(df$x, FALSE)
    df$height <- df$height %||% params$height %||% resolution(df$y, FALSE)

    transform(df,
      xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  draw_groups = function(data,  scales, coordinates, ...) {
    # data$colour[is.na(data$colour)] <- data$fill[is.na(data$colour)]
    GeomRect$draw_groups(data, scales, coordinates, ...)
  },

  default_aes = aes(fill = "grey20", colour = NA, size = 0.1, linetype = 1,
    alpha = NA),

  required_aes = c("x", "y"),

  draw_key = draw_key_rect
)
