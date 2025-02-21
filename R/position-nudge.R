#' Nudge points a fixed distance
#'
#' `position_nudge()` is generally useful for adjusting the position of
#' items on discrete scales by a small amount. Nudging is built in to
#' [geom_text()] because it's so useful for moving labels a small
#' distance from what they're labelling.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move.
#' @export
#' @eval rd_aesthetics("position", "nudge")
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
#'
#' # For each text individually
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_text(aes(label = y, nudge_y = c(-0.1, 0.1, -0.1, 0.1)))
position_nudge <- function(x = NULL, y = NULL) {
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
  x = NULL,
  y = NULL,

  default_aes = aes(nudge_x = 0, nudge_y = 0),

  setup_params = function(self, data) {
    list(
      x = self$x %||% data$nudge_x,
      y = self$y %||% data$nudge_y
    )
  },

  compute_layer = function(self, data, params, layout) {
    trans_x <- if (any(params$x != 0)) function(x) x + params$x
    trans_y <- if (any(params$y != 0)) function(y) y + params$y
    transform_position(data, trans_x, trans_y)
  }
)
