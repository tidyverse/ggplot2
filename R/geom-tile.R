#' Rectangles
#'
#' `geom_rect()` and `geom_tile()` do the same thing, but are
#' parameterised differently: `geom_rect()` uses the locations of the four
#' corners (`xmin`, `xmax`, `ymin` and `ymax`), while
#' `geom_tile()` uses the center of the tile and its size (`x`,
#' `y`, `width`, `height`). `geom_raster()` is a high
#' performance special case for when all the tiles are the same size, and no
#' pattern fills are applied.
#'
#' @eval rd_aesthetics("geom", "tile", "Note that `geom_raster()` ignores `colour`.")
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams geom_segment
#' @export
#'
#' @details
#' `geom_rect()` and `geom_tile()`'s respond differently to scale
#' transformations due to their parameterisation. In `geom_rect()`, the scale
#' transformation is applied to the corners of the rectangles. In `geom_tile()`,
#' the transformation is applied only to the centres and its size is determined
#' after transformation.
#'
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
#'   geom_tile(aes(fill = z), colour = "grey50")
#' ggplot(df, aes(x, y, width = w)) +
#'   geom_tile(aes(fill = z), colour = "grey50")
#' ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
#'   geom_rect(aes(fill = z), colour = "grey50")
#'
#' \donttest{
#' # Justification controls where the cells are anchored
#' df <- expand.grid(x = 0:5, y = 0:5)
#' set.seed(1)
#' df$z <- runif(nrow(df))
#' # default is compatible with geom_tile()
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_raster()
#' # zero padding
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_raster(hjust = 0, vjust = 0)
#'
#' # Inspired by the image-density plots of Ken Knoblauch
#' cars <- ggplot(mtcars, aes(mpg, factor(cyl)))
#' cars + geom_point()
#' cars + stat_bin_2d(aes(fill = after_stat(count)), binwidth = c(3,1))
#' cars + stat_bin_2d(aes(fill = after_stat(density)), binwidth = c(3,1))
#'
#' cars +
#'   stat_density(
#'     aes(fill = after_stat(density)),
#'     geom = "raster",
#'     position = "identity"
#'    )
#' cars +
#'   stat_density(
#'     aes(fill = after_stat(count)),
#'     geom = "raster",
#'     position = "identity"
#'   )
#' }
geom_tile <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      linejoin = "mitre",
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
    params = list2(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.R
GeomTile <- ggproto("GeomTile", GeomRect,
  extra_params = c("na.rm"),

  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

    transform(data,
      xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  default_aes = aes(fill = "grey20", colour = NA, linewidth = 0.1, linetype = 1,
    alpha = NA, width = NA, height = NA),

  required_aes = c("x", "y"),

  # These aes columns are created by setup_data(). They need to be listed here so
  # that GeomRect$handle_na() properly removes any bars that fall outside the defined
  # limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

  draw_key = draw_key_polygon
)
