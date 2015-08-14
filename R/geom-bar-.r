#' Bars, rectangles with bases on x-axis
#'
#' There are two types of bar charts, determined by what is mapped to bar
#' height. By default, \code{geom_bar} uses \code{stat = "bar"} which makes the
#' height of the bar proportion to the number of cases in each group (or
#' if the \code{weight} aethetic is supplied, the sum of the weights).
#  If you want the heights of the bars to represent values in the data, use
#' \code{stat="identity"} and map a variable to the \code{y} aesthetic.
#'
#' A bar chart maps the height of the bar to a variable, and so the base of
#' the bar must always be shown to produce a valid visual comparison.
#' Naomi Robbins has a nice
#' \href{http://www.b-eye-network.com/view/index.php?cid=2468}{article on this topic}.
#' This is why it doesn't make sense to use a log-scaled y axis with a bar chart.
#'
#' By default, multiple x's occurring in the same place will be stacked atop
#' one another by \code{\link{position_stack}}. If you want them to be dodged
#' side-to-side, see \code{\link{position_dodge}}. Finally,
#' \code{\link{position_fill}} shows relative proportions at each x by stacking
#' the bars and then stretching or squashing to the same height.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "bar")}
#'
#' @seealso \code{\link{geom_histogram}} for continuous data,
#'   \code{\link{position_dodge}} for creating side-by-side barcharts.
#' @export
#' @inheritParams geom_point
#' @param width Bar width. By default, set of 90% of the resolution of the
#'   data.
#' @param geom,stat Override the default connection between \code{geom_bar} and
#'   \code{stat_bar}.
#' @param orient Can be \code{"h"} or \code{"v"}. Indicates whether
#'   the Geom should display horizontally or vertically.
#' @examples
#' # geom_bar is designed to make it easy to create bar charts that show
#' # counts (or sums of weights)
#' g <- ggplot(mpg, aes(class))
#' # Number of cars in each class:
#' g + geom_bar()
#' # Total engine displacement of each class
#' g + geom_bar(aes(weight = displ))
#'
#' # To show (e.g.) means, you need stat = "identity"
#' df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
#' ggplot(df, aes(trt, outcome)) +
#'   geom_bar(stat = "identity")
#' # But geom_point() display exactly the same information and doesn't
#' # require the y-axis to touch zero.
#' ggplot(df, aes(trt, outcome)) +
#'   geom_point()
#'
#' # You can also use geom_bar() with continuous data, in which case
#' # it will show counts at unique locations
#' df <- data.frame(x = rep(c(2.9, 3.1, 4.5), c(5, 10, 4)))
#' ggplot(df, aes(x)) + geom_bar()
#' # cf. a histogram of the same data
#' ggplot(df, aes(x)) + geom_histogram(binwidth = 0.5)
#'
#' \donttest{
#' # Bar charts are automatically stacked when multiple bars are placed
#' # at the same location
#' g + geom_bar(aes(fill = drv))
#'
#' # You can instead dodge, or fill them
#' g + geom_bar(aes(fill = drv), position = "dodge")
#' g + geom_bar(aes(fill = drv), position = "fill")
#'
#' # To change plot order of bars, change levels in underlying factor
#' reorder_size <- function(x) {
#'   factor(x, levels = names(sort(table(x))))
#' }
#' ggplot(mpg, aes(reorder_size(class))) + geom_bar()
#' }
geom_bar <- function(mapping = NULL, data = NULL, stat = "bar",
                     position = "stack", orient = "v", width = NULL, ...,
                     show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    stat_params = list(width = width),
    geom = GeomBar,
    position = position,
    flip = orient == "h",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...),
    geom_params = list(orient = orient)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBar <- ggproto("GeomBar", Geom,
  required_aes = "x",
  default_aes = aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1,
                    weight = 1, alpha = NA),

  reparameterise = function(df, params) {
    df$width <- df$width %||%
      params$width %||% (resolution(df$x, FALSE) * 0.9)
    transform(df,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
  },

  draw = function(data, scales, coordinates, ...) {
    GeomRect$draw(data, scales, coordinates, ...)
  },

  draw_key = draw_key_polygon
)

#' @export
#' @rdname geom_bar
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{groupwise proportion}
#' }
stat_bar <- function(mapping = NULL, data = NULL, geom = "bar",
                     position = "stack", width = NULL, ...,
                     show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBar,
    stat_params = list(width = width),
    geom = geom,
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
#' @include stat-.r
StatBar <- ggproto("StatBar", Stat,
  required_aes = "x",
  default_aes = aes(y = ..count..),

  compute_group = function(self, data, ..., width = NULL) {
    x <- data$x
    weight <- data$weight %||% rep(1, length(x))
    width <- width %||% resolution(x) * 0.9

    count <- as.numeric(tapply(weight, x, sum, na.rm = TRUE))
    count[is.na(count)] <- 0

    data.frame(
      count = count,
      prop = count / sum(abs(count)),
      x = unique(x),
      width = width
    )
  }
)
