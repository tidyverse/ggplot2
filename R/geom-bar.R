#' Bar charts
#'
#' There are two types of bar charts: `geom_bar()` and `geom_col()`.
#' `geom_bar()` makes the height of the bar proportional to the number of
#' cases in each group (or if the `weight` aesthetic is supplied, the sum
#' of the weights). If you want the heights of the bars to represent values
#' in the data, use `geom_col()` instead. `geom_bar()` uses `stat_count()` by
#' default: it counts the number of cases at each x position. `geom_col()`
#' uses `stat_identity()`: it leaves the data as is.
#'
#' A bar chart uses height to represent a value, and so the base of the
#' bar must always be shown to produce a valid visual comparison.
#' Proceed with caution when using transformed scales with a bar chart.
#' It's important to always use a meaningful reference point for the base of the bar.
#' For example, for log transformations the reference point is 1. In fact, when
#' using a log scale, `geom_bar()` automatically places the base of the bar at 1.
#' Furthermore, never use stacked bars with a transformed scale, because scaling
#' happens before stacking. As a consequence, the height of bars will be wrong
#' when stacking occurs with a transformed scale.
#'
#' By default, multiple bars occupying the same `x` position will be stacked
#' atop one another by [position_stack()]. If you want them to be dodged
#' side-to-side, use [position_dodge()] or [position_dodge2()]. Finally,
#' [position_fill()] shows relative proportions at each `x` by stacking the
#' bars and then standardising each bar to have the same height.
#'
#' @eval rd_orientation()
#'
#' @eval rd_aesthetics("geom", "bar")
#' @eval rd_aesthetics("geom", "col")
#' @eval rd_aesthetics("stat", "count")
#' @seealso
#'   [geom_histogram()] for continuous data,
#'   [position_dodge()] and [position_dodge2()] for creating side-by-side
#'   bar charts.
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @param orientation The orientation of the layer. The default (`NA`)
#' automatically determines the orientation from the aesthetic mapping. In the
#' rare event that this fails it can be given explicitly by setting `orientation`
#' to either `"x"` or `"y"`. See the *Orientation* section for more detail.
#' @param just Adjustment for column placement. Set to `0.5` by default, meaning
#'   that columns will be centered about axis breaks. Set to `0` or `1` to place
#'   columns to the left/right of axis breaks. Note that this argument may have
#'   unintended behaviour when used with alternative positions, e.g.
#'   `position_dodge()`.
#' @param width Bar width. By default, set to 90% of the [resolution()] of the
#'   data.
#' @param geom,stat Override the default connection between `geom_bar()` and
#'   `stat_count()`.
#' @examples
#' # geom_bar is designed to make it easy to create bar charts that show
#' # counts (or sums of weights)
#' g <- ggplot(mpg, aes(class))
#' # Number of cars in each class:
#' g + geom_bar()
#' # Total engine displacement of each class
#' g + geom_bar(aes(weight = displ))
#' # Map class to y instead to flip the orientation
#' ggplot(mpg) + geom_bar(aes(y = class))
#'
#' # Bar charts are automatically stacked when multiple bars are placed
#' # at the same location. The order of the fill is designed to match
#' # the legend
#' g + geom_bar(aes(fill = drv))
#'
#' # If you need to flip the order (because you've flipped the orientation)
#' # call position_stack() explicitly:
#' ggplot(mpg, aes(y = class)) +
#'  geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
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
#'
#' # Use `just` to control how columns are aligned with axis breaks:
#' df <- data.frame(x = as.Date(c("2020-01-01", "2020-02-01")), y = 1:2)
#' # Columns centered on the first day of the month
#' ggplot(df, aes(x, y)) + geom_col(just = 0.5)
#' # Columns begin on the first day of the month
#' ggplot(df, aes(x, y)) + geom_col(just = 1)
geom_bar <- function(mapping = NULL, data = NULL,
                     stat = "count", position = "stack",
                     ...,
                     just = 0.5,
                     width = NULL,
                     na.rm = FALSE,
                     orientation = NA,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      just = just,
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.R
GeomBar <- ggproto("GeomBar", GeomRect,
  required_aes = c("x", "y"),

  # These aes columns are created by setup_data(). They need to be listed here so
  # that GeomRect$handle_na() properly removes any bars that fall outside the defined
  # limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },

  extra_params = c("just", "na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    data$just <- params$just %||% 0.5
    data <- transform(data,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width * just, xmax = x + width * (1 - just),
      width = NULL, just = NULL
    )
    flip_data(data, params$flipped_aes)
  },

  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", width = NULL, flipped_aes = FALSE) {
    # Hack to ensure that width is detected as a parameter
    ggproto_parent(GeomRect, self)$draw_panel(
      data,
      panel_params,
      coord,
      lineend = lineend,
      linejoin = linejoin
    )
  },
  rename_size = TRUE
)
