#' Tile plane with rectangles.
#'
#' Similar to \code{\link{levelplot}} and \code{\link{image}}.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "tile")}
#'
#' @inheritParams geom_point
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
#' p <- ggplot(pp(20), aes(x=x,y=y))
#'
#' p + geom_tile() #pretty useless!
#'
#' # Add aesthetic mappings
#' p + geom_tile(aes(fill=z))
#'
#' # Change scale
#' p + geom_tile(aes(fill=z)) + scale_fill_gradient(low="green", high="red")
#'
#' # Missing values
#' p <- ggplot(pp(20)[sample(20*20, size=200),], aes(x=x,y=y,fill=z))
#' p + geom_tile()
#'
#' # Input that works with image
#' image(t(volcano)[ncol(volcano):1,])
#' library(reshape2) # for melt
#' ggplot(melt(volcano), aes(x=Var1, y=Var2, fill=value)) + geom_tile()
#'
#' # inspired by the image-density plots of Ken Knoblauch
#' cars <- ggplot(mtcars, aes(mpg, factor(cyl)))
#' cars + geom_point()
#' cars + stat_bin2d(aes(fill = ..count..), binwidth = c(3,1))
#' cars + stat_bin2d(aes(fill = ..density..), binwidth = c(3,1))
#'
#' cars + stat_density(aes(fill = ..density..), geom = "tile", position = "identity")
#' cars + stat_density(aes(fill = ..count..), geom = "tile", position = "identity")
#'
#' # Another example with with unequal tile sizes
#' x.cell.boundary <- c(0, 4, 6, 8, 10, 14)
#' example <- data.frame(
#'   x = rep(c(2, 5, 7, 9, 12), 2),
#'   y = factor(rep(c(1,2), each=5)),
#'   z = rep(1:5, each=2),
#'   w = rep(diff(x.cell.boundary), 2)
#' )
#'
#' ggplot(example, aes(x, y)) + geom_tile(aes(fill = z))
#' ggplot(example, aes(x, y)) + geom_tile(aes(fill = z, width = w))
#' ggplot(example, aes(x, y)) + geom_tile(aes(fill = factor(z), width = w))
#'
#' # You can manually set the colour of the tiles using
#' # scale_manual
#' col <- c("darkblue", "blue", "green", "orange", "red")
#' ggplot(example, aes(x, y)) +
#'   geom_tile(aes(fill = col[z], width = w, group = 1)) +
#'   scale_fill_identity(labels = letters[1:5], breaks = col)
#' }
geom_tile <- function (mapping = NULL, data = NULL, stat = "identity",
  position = "identity", show_guide = NA, inherit.aes = TRUE, ...)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTile,
    position = position,
    show_guide = show_guide,
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

  draw_key = draw_key_polygon
)
