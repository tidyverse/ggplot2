#' Bar charts
#'
#' There are two types of bar charts: `geom_bar` makes the height of the
#' bar proportional to the number of cases in each group (or if the
#' `weight` aesthetic is supplied, the sum of the weights). If you want the
#' heights of the bars to represent values in the data, use
#' \link{geom_col} instead. `geom_bar` uses `stat_count` by
#' default: it counts the  number of cases at each x position. `geom_col`
#' uses `stat_identity`: it leaves the data as is.
#'
#' A bar chart uses height to represent a value, and so the base of the
#' bar must always be shown to produce a valid visual comparison. Naomi Robbins
#' has a nice
#' \href{http://www.b-eye-network.com/view/index.php?cid=2468}{article on this
#' topic}. This is why it doesn't make sense to use a log-scaled y axis with a
#' bar chart.
#'
#' By default, multiple bar occupying the same `x` position will be stacked atop
#' one another by [position_stack()]. If you want them to be dodged
#' side-to-side, use [position_dodge()] or [position_dodge2()]. Finally,
#' [position_fill()] shows relative proportions at each `x` by stacking the bars
#' and then standardising each bar to have the same height.
#'
#' @section Aesthetics:
#' \aesthetics{geom}{bar}
#'
#' @seealso
#'   [geom_histogram()] for continuous data,
#'   [position_dodge()] and [position_dodge2()] for creating side-by-side
#'   barcharts.
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @param width Bar width. By default, set to 90\% of the resolution of the data.
#' @param binwidth `geom_bar` no longer has a binwidth argument - if
#'   you use it you'll get an warning telling to you use
#'   [geom_histogram()] instead.
#' @param geom,stat Override the default connection between `geom_bar` and
#'   `stat_count`.
#' @examples
#' # geom_bar is designed to make it easy to create bar charts that show
#' # counts (or sums of weights)
#' g <- ggplot(mpg, aes(class))
#' # Number of cars in each class:
#' g + geom_bar()
#' # Total engine displacement of each class
#' g + geom_bar(aes(weight = displ))
#'
#' # Bar charts are automatically stacked when multiple bars are placed
#' # at the same location. The order of the fill is designed to match
#' # the legend
#' g + geom_bar(aes(fill = drv))
#'
#' # If you need to flip the order (because you've flipped the plot)
#' # call position_stack() explicitly:
#' g +
#'  geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
#'  coord_flip() +
#'  theme(legend.position = "top")
#'
#' # To show (e.g.) means, you need geom_col()
#' df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
#' ggplot(df, aes(trt, outcome)) +
#'   geom_col()
#' # But geom_point() displays exactly the same information and doesn't
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
geom_bar <- function(mapping = NULL, data = NULL,
                     stat = "count", position = "stack",
                     ...,
                     width = NULL,
                     binwidth = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  if (!is.null(binwidth)) {
    warning("`geom_bar()` no longer has a `binwidth` parameter. ",
      "Please use `geom_histogram()` instead.", call. = "FALSE")
    return(geom_histogram(mapping = mapping, data = data,
      position = position, width = width, binwidth = binwidth, ...,
      na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes))
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
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
GeomBar <- ggproto("GeomBar", GeomRect,
  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    transform(data,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
  },

  draw_panel = function(self, data, panel_params, coord, width = NULL) {
    # Hack to ensure that width is detected as a parameter
    ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord)
  }
)
