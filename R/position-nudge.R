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
  # find the appropriate position adjustment for the given x and y settings
  # not nudging in dimensions that are not required reduces the risk
  # spurious breaks, e.g. when the geom only has an x coordinate but no
  # y coordinate
  if (x != 0) {
    if (y != 0) {
      pos <- ggproto(NULL, PositionNudge, x = x, y = y)
    } else {
      pos <- ggproto(NULL, PositionNudgeX, x = x)
    }
  } else if (y != 0) {
    pos <- ggproto(NULL, PositionNudgeY, y = y)
  } else {
    pos <- ggproto(NULL, PositionIdentity)
  }

  pos
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionNudge <- ggproto("PositionNudge", Position,
  x = 0,
  y = 0,

  required_aes = c("x", "y"),

  setup_params = function(self, data) {
    list(x = self$x, y = self$y)
  },

  compute_layer = function(data, params, panel) {
    transform_position(data, function(x) x + params$x, function(y) y + params$y)
  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionNudgeX <- ggproto("PositionNudgeX", Position,
  x = 0,

  required_aes = c("x"),

  setup_params = function(self, data) {
    list(x = self$x)
  },

  compute_layer = function(data, params, panel) {
    transform_position(data, function(x) x + params$x, NULL)
  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionNudgeY <- ggproto("PositionNudgeY", Position,
  y = 0,

  required_aes = c("y"),

  setup_params = function(self, data) {
    list(y = self$y)
  },

  compute_layer = function(data, params, panel) {
    transform_position(data, NULL, function(y) y + params$y)
  }
)

