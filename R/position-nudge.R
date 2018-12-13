#' Nudge points a fixed distance
#'
#' `position_nudge` is generally useful for adjusting the position of
#' items on discrete scales by a small amount. Nudging is built in to
#' [geom_text()] because it's so useful for moving labels a small
#' distance from what they're labelling.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move.
#' @export
#' @examples
#' df <- data.frame(
#'   x = c(1,3,2,5),
#'   y = c("a","c","d","c")
#' )
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text(aes(label = y))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text(aes(label = y), position = position_nudge(y = -0.1))
#'
#' # Or, in brief
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text(aes(label = y), nudge_y = -0.1)
position_nudge <- function(x = 0, y = 0) {
  ggproto(NULL, PositionNudge,
    x = x,
    y = y
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionNudge <- ggproto("PositionNudge", Position,
  x = 0,
  y = 0,

  setup_params = function(self, data) {
    list(x = self$x, y = self$y)
  },

  compute_layer = function(data, params, panel) {
    # transform only the dimensions for which non-zero nudging is requested
    if (any(params$x != 0)) {
      if (any(params$y != 0)) {
        transform_position(data, function(x) x + params$x, function(y) y + params$y)
      } else {
        transform_position(data, function(x) x + params$x, NULL)
      }
    } else if (any(params$y != 0)) {
      transform_position(data, NULL, function(y) y + params$y)
    } else {
      data # if both x and y are 0 we don't need to transform
    }
  }
)
