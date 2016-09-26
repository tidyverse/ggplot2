#' Bars, rectangles with bases on x-axis
#'
#' This is an alternate version of \code{geom_bar} that uses
#' \code{stat="identity"} (i.e. the heights of the bars represent values in the
#' data) by default, requiring the use of a \code{y} aesthetic.
#' If you need \code{stat="count"} functionality, use \code{geom_bar}.
#'
#' A bar chart maps the height of the bar to a variable, and so the base of the
#' bar must always be shown to produce a valid visual comparison. Naomi Robbins
#' has a nice
#' \href{http://www.b-eye-network.com/view/index.php?cid=2468}{article on this
#' topic}. This is why it doesn't make sense to use a log-scaled y axis with a
#' bar chart.
#'
#' By default, multiple x's occurring in the same place will be stacked atop one
#' another by \code{\link{position_stack}}. If you want them to be dodged
#' side-to-side, see \code{\link{position_dodge}}. Finally,
#' \code{\link{position_fill}} shows relative proportions at each x by stacking
#' the bars and then stretching or squashing to the same height.
#'
#' @section Aesthetics:
#' \aesthetics{geom}{col}
#'
#' @seealso \code{\link{geom_bar}} to make the height of the bar proportional to
#'  the number of cases in each group or sum of weights,
#'   \code{\link{geom_histogram}} for continuous data,
#'   \code{\link{position_dodge}} for creating side-by-side barcharts.
#' @export
#' @inheritParams layer
#' @inheritParams geom_point
#' @param width Bar width. By default, set to 90\% of the resolution of the data.
#' @examples
#' df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
#' ggplot(df, aes(trt, outcome)) +
#'   geom_col()
#' # But geom_point() displays exactly the same information and doesn't
#' # require the y-axis to touch zero.
#' ggplot(df, aes(trt, outcome)) +
#'   geom_point()
geom_col <- function(mapping = NULL, data = NULL,
                     position = "stack",
                     ...,
                     width = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomCol,
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
GeomCol <- ggproto("GeomCol", GeomRect,
  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    transform(data,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
  },

  draw_panel = function(self, data, panel_scales, coord, width = NULL) {
    # Hack to ensure that width is detected as a parameter
    ggproto_parent(GeomRect, self)$draw_panel(data, panel_scales, coord)
  }
)
